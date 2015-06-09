<?php
/** 
 * Parser to parse Astoria URI $filter option 
 * 
 * PHP version 5.3
 * 
 * @category  ODataPHPProd
 * @package   ODataProducer_UriProcessor_QueryProcessor_ExpressionParser
 * @author    Microsoft Open Technologies, Inc. <msopentech@microsoft.com>
 * @copyright Microsoft Open Technologies, Inc.
 * @license   New BSD license, (http://www.opensource.org/licenses/bsd-license.php)
 * @version   GIT: 1.2
 * @link      https://github.com/MSOpenTech/odataphpprod
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer.
 *  Redistributions in binary form must reproduce the above copyright notice, this
 *  list of conditions  and the following disclaimer in the documentation and/or
 *  other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A  PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
namespace ODataProducer\UriProcessor\QueryProcessor\ExpressionParser;
use ODataProducer\Common\Messages;
use ODataProducer\Common\ODataConstants;
use ODataProducer\Common\ODataException;
use ODataProducer\Providers\Metadata\Type\Boolean;
use ODataProducer\Providers\Metadata\Type\DateTime;
use ODataProducer\Providers\Metadata\Type\Decimal;
use ODataProducer\Providers\Metadata\Type\String;
use ODataProducer\Providers\Metadata\Type\Int64;
use ODataProducer\Providers\Metadata\Type\Int32;
use ODataProducer\Providers\Metadata\Type\Double;
use ODataProducer\Providers\Metadata\Type\Single;
use ODataProducer\Providers\Metadata\Type\Guid;
use ODataProducer\Providers\Metadata\Type\Binary;
use ODataProducer\Providers\Metadata\Type\Null1;
use ODataProducer\Providers\Metadata\Type\INavigationType;
use ODataProducer\Providers\Metadata\Type\TypeCode;
use ODataProducer\Providers\Metadata\Type\IType;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\AbstractExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\SimplePropertyAccessExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\ArithmeticExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\RelationalExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\LogicalExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\FunctionCallExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\UnaryExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\ConstantExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\ExpressionType;
use ODataProducer\UriProcessor\QueryProcessor\FunctionDescription\FunctionDescription;
use ODataProducer\Common\NotImplementedException;
/**
 * $filter option parser.
 * 
 * @category  ODataPHPProd
 * @package   ODataProducer_UriProcessor_QueryProcessor_ExpressionParser
 * @author    Microsoft Open Technologies, Inc. <msopentech@microsoft.com>
 * @copyright Microsoft Open Technologies, Inc.
 * @license   New BSD license, (http://www.opensource.org/licenses/bsd-license.php)
 * @version   GIT: 1.2
 * @link      https://github.com/MSOpenTech/odataphpprod
 */
class ExpressionParserSimple
{
    const RECURSION_LIMIT = 200;

    /**     
     * The Lexical analyzer
     * 
     * @var ExpressionLexer
     */
    private $_lexer;
    
    /**     
     * The current recursion depth
     * 
     * @var int
     */
    private $_recursionDepth;

    /**
     * Construct a new instance of ExpressionParser
     * 
     * @param string       $text                       The expression to parse.
     *
     * Note: TODO Expression parser should not depends on the fact that end user is implmenting
     * IExpressionProider or not.
     */
    public function __construct($text)
    {
        $this->_lexer = new ExpressionLexer($text);
    }


    /**  
     * Get the current token from lexer
     * 
     * @return ExpressionToken
     */
    private function _getCurrentToken()
    {
        return $this->_lexer->getCurrentToken();
    }

    /** 
     * Set the current token in lexer
     * 
     * @param ExpressionToken $token The token to set as current token.
     * 
     * @return void
     */
    private function _setCurrentToken($token)
    {
        $this->_lexer->setCurrentToken($token);
    }  

    /**     
     * Resets parser with new expression string.
     * 
     * @param string $text Reset the expression to parse.
     * 
     * @return void
     */
    public function resetParser($text)
    {
        $this->_lexer = new ExpressionLexer($text);
        $this->_recursionDepth = 0;
    }

    /** 
     * Parse the expression in filter option.
     * 
     * @return AbstractExpression
     */
    public function parseFilter()
    {
        return $this->_parseExpression();
    }

    /**
     * Start parsing the expression
     * 
     * @return AbstractExpression
     */
    private function _parseExpression()
    {
        $this->_recurseEnter();
        $expr = $this->_parseLogicalOr();
        $this->_recurseLeave();
        return $expr;
    }

    /** 
     * Parse logical or (or)
     * 
     * @return AbstractExpression
     */
    private function _parseLogicalOr()
    {
        $this->_recurseEnter();
        $left = $this->_parseLogicalAnd();
        while ($this->_tokenIdentifierIs(ODataConstants::KEYWORD_OR)) {
            $logicalOpToken = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();
            $right = $this->_parseLogicalAnd();
            $left = new LogicalExpression(
                $left, $right, ExpressionType::OR_LOGICAL
            );
        }

        $this->_recurseLeave();
        return $left;
    }

    /**
     * Parse logical and (and).
     * 
     * @return AbstractExpression
     */
    private function _parseLogicalAnd()
    {
        $this->_recurseEnter();
        $left = $this->_parseComparison();
        while ($this->_tokenIdentifierIs(ODataConstants::KEYWORD_AND)) {
            $logicalOpToken = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();
            $right = $this->_parseComparison();
            $left = new LogicalExpression(
                $left, $right, 
                ExpressionType::AND_LOGICAL
            );
        }

        $this->_recurseLeave();
        return $left;
    }

    /**
     * Parse comparison operation (eq, ne, gt, ge, lt, le)
     * 
     * @return AbstractExpression
     */
    private function _parseComparison()
    {
        $this->_recurseEnter();
        $left = $this->_parseAdditive();
        while ($this->_getCurrentToken()->isComparisonOperator()) {
            $comparisonToken = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();
            $right = $this->_parseAdditive();
            $left = self::_generateComparisonExpression(
                $left, $right, 
                $comparisonToken
            );
        }

        $this->_recurseLeave();
        return $left;
    }

    /**
     * Parse additive operation (add, sub).
     * 
     * @return AbstractExpression
     */
    private function _parseAdditive()
    {
        $this->_recurseEnter();
        $left = $this->_parseMultiplicative();
        while ($this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_ADD) 
            || $this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_SUB)) {
            $additiveToken = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();
            $right = $this->_parseMultiplicative();

            if ($additiveToken->identifierIs(ODataConstants::KEYWORD_ADD)) {
                $left = new ArithmeticExpression(
                    $left, $right, 
                    ExpressionType::ADD
                );
            } else {
                $left = new ArithmeticExpression(
                    $left, $right, 
                    ExpressionType::SUBTRACT
                );
            }
        }

        $this->_recurseLeave();
        return $left;
    }

    /** 
     * Parse multipicative operators (mul, div, mod)
     * 
     * @return AbstractExpression
     */
    private function  _parseMultiplicative()
    {
        $this->_recurseEnter();
        $left = $this->_parseUnary();
        while ($this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_MULTIPLY) 
            || $this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_DIVIDE) 
            || $this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_MODULO)
        ) {
            $multiplicativeToken = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();
            $right = $this->_parseUnary();

            if ($multiplicativeToken->identifierIs(ODataConstants::KEYWORD_MULTIPLY)) {
                $left = new ArithmeticExpression(
                    $left, $right, ExpressionType::MULTIPLY
                );
            } else if ($multiplicativeToken->identifierIs(ODataConstants::KEYWORD_DIVIDE)) {
                $left = new ArithmeticExpression(
                    $left, $right, ExpressionType::DIVIDE
                );
            } else {                
                $left = new ArithmeticExpression(
                    $left, $right, ExpressionType::MODULO
                );
            }
        }

        $this->_recurseLeave();
        return $left;
    }

    /**
     * Parse unary operator (- ,not)
     * 
     * @return AbstractExpression
     */
    private function _parseUnary()
    {
        $this->_recurseEnter();
       
        if ($this->_getCurrentToken()->Id == ExpressionTokenId::MINUS 
            || $this->_getCurrentToken()->identifierIs(ODataConstants::KEYWORD_NOT)
        ) {
            $op = clone $this->_getCurrentToken();
            $this->_lexer->nextToken();            
            if ($op->Id == ExpressionTokenId::MINUS 
                && (ExpressionLexer::isNumeric($this->_getCurrentToken()->Id))
            ) {
                $numberLiteral = $this->_getCurrentToken();
                $numberLiteral->Text = '-' . $numberLiteral->Text;
                $numberLiteral->Position = $op->Position;
                $v = $this->_getCurrentToken();
                $this->_setCurrentToken($numberLiteral);
                $this->_recurseLeave();
                return $this->_parsePrimary();
            }

            $expr = $this->_parsePrimary();

            if ($op->Id == ExpressionTokenId::MINUS) {
                $expr = new UnaryExpression(
                    $expr, ExpressionType::NEGATE, $expr->getType()
                );
            } else {
                $expr = new UnaryExpression(
                    $expr, ExpressionType::NOT_LOGICAL, new Boolean()
                );
            }

            $this->_recurseLeave();
            return $expr;
        }

        $this->_recurseLeave();
        return $this->_parsePrimary();
    }

    /**
     * Start parsing the primary.
     *  
     * @return AbstractExpression
     */
    private function _parsePrimary()
    {
        $this->_recurseEnter();
        $expr = $this->_parsePrimaryStart();
        while (true) {
            if ($this->_getCurrentToken()->Id == ExpressionTokenId::SLASH) {
                $this->_lexer->nextToken();
                $expr = $this->_parsePropertyAccess($expr);
            } else {
                break;
            }
        }

        $this->_recurseLeave();
        return $expr;
    }

    /** 
     * Parse primary tokens [literals, identifiers (e.g. function call), open 
     * param for sub expressions]
     * 
     * @return AbstractExpression
     */
    private function _parsePrimaryStart()
    {
        switch ($this->_lexer->getCurrentToken()->Id) {
        case ExpressionTokenId::BOOLEAN_LITERAL:
            return $this->_parseTypedLiteral(new Boolean());
        case ExpressionTokenId::DATETIME_LITERAL:
            return $this->_parseTypedLiteral(new DateTime());
        case ExpressionTokenId::DECIMAL_LITERAL:
            return $this->_parseTypedLiteral(new Decimal());
        case ExpressionTokenId::NULL_LITERAL:
            return $this->_parseNullLiteral();
        case ExpressionTokenId::IDENTIFIER:
            return $this->_parseIdentifier();
        case ExpressionTokenId::STRING_LITERAL:
            return $this->_parseTypedLiteral(new String());
        case ExpressionTokenId::INT64_LITERAL:
            return $this->_parseTypedLiteral(new Int64());
        case ExpressionTokenId::INTEGER_LITERAL:
            return $this->_parseTypedLiteral(new Int32());
        case ExpressionTokenId::DOUBLE_LITERAL:
            return $this->_parseTypedLiteral(new Double());
        case ExpressionTokenId::SINGLE_LITERAL:
            return $this->_parseTypedLiteral(new Single());
        case ExpressionTokenId::GUID_LITERAL:
            return $this->_parseTypedLiteral(new Guid());
        case ExpressionTokenId::BINARY_LITERAL:
            throw new NotImplementedException(
                'Support for binary is not implemented'
            );
            return $this->_parseTypedLiteral(new Binary());
        case ExpressionTokenId::OPENPARAM:
            return $this->_parseParenExpression();
        default:
            ODataException::createSyntaxError("Expression expected.");
        }
    }

    /** 
     * Parse Sub expression.
     * 
     * @return AbstractExpression
     */
    private function _parseParenExpression()
    {
        if ($this->_getCurrentToken()->Id != ExpressionTokenId::OPENPARAM) {
            ODataException::createSyntaxError("Open parenthesis expected.");
        }

        $this->_lexer->nextToken();
        $expr = $this->_parseExpression();
        if ($this->_getCurrentToken()->Id != ExpressionTokenId::CLOSEPARAM) {
            ODataException::createSyntaxError("Close parenthesis expected.");
        }

        $this->_lexer->nextToken();
        return $expr;
    }

    /**
     * Parse an identifier
     * 
     * @return AbstractExpression
     */
    private function _parseIdentifier()
    {
        $this->_validateToken(ExpressionTokenId::IDENTIFIER);

        // An open paren here would indicate calling a method        
        $identifierIsFunction 
            = $this->_lexer->peekNextToken()->Id == ExpressionTokenId::OPENPARAM;
        if ($identifierIsFunction) {
            return $this->_parseIdentifierAsFunction();
        } else {
            return $this->_parsePropertyAccess(null);
        }
    }

    /**
     * Parse a property access
     * 
     * @param PropertyAccessExpression $parentExpression Parent expression.
     * 
     * @throws ODataException
     * 
     * @return PropertyAccessExpression
     */
    private function _parsePropertyAccess($parentExpression)
    {
        $property = $this->_getCurrentToken()->getIdentifier(); 

        $exp = new SimplePropertyAccessExpression($parentExpression, $property); 
        $this->_lexer->nextToken();
        return $exp;
    }

    /** 
     * Try to parse an identifier which is followed by an opern bracket as
     * astoria URI function call.
     * 
     * @return AbstractExpression
     * 
     * @throws ODataException
     */
    private function _parseIdentifierAsFunction()
    {
        $functionToken = clone $this->_getCurrentToken();
        $this->_lexer->nextToken();
        $paramExpressions = $this->_parseArgumentList();
        $function = new FunctionDescription($functionToken->getIdentifier(), '', '');
        return new FunctionCallExpression($function, $paramExpressions);
    }

    /**
     * Start parsing argument list of a function-call
     * 
     * @return array<AbstractExpression>
     */
    private function _parseArgumentList()
    {
        if ($this->_getCurrentToken()->Id != ExpressionTokenId::OPENPARAM) {
            ODataException::createSyntaxError("Open parenthesis expected.");
        }

        $this->_lexer->nextToken();
        $args 
            = $this->_getCurrentToken()->Id != ExpressionTokenId::CLOSEPARAM 
             ? $this->_parseArguments() : array();
        if ($this->_getCurrentToken()->Id != ExpressionTokenId::CLOSEPARAM) {
            ODataException::createSyntaxError("Close parenthesis expected.");
        }

        $this->_lexer->nextToken();
        return $args;
    }

    /** 
     * Parse arguments of  a function-call.
     * 
     * @return array<AbstractExpression>
     */
    private function _parseArguments()
    {
        $argList = array();
        while (true) {
            $argList[] = $this->_parseExpression();
            if ($this->_getCurrentToken()->Id != ExpressionTokenId::COMMA) {
                break;
            }

            $this->_lexer->nextToken();
        }

        return $argList;
    }

    /**
     * Parse primitive type literal.
     * 
     * @param IType $targetType Expected type of the current literal.
     * 
     * @return AbstractExpression
     * 
     * @throws ODataException
     */
    private function _parseTypedLiteral(IType $targetType)
    {
        $literal = $this->_lexer->getCurrentToken()->Text;
        $outVal = null;
        if (!$targetType->validate($literal, $outVal)) {
            ODataException::createSyntaxError(
                Messages::expressionParserUnrecognizedLiteral(
                    $targetType->getFullTypeName(), 
                    $literal, 
                    $this->_lexer->getCurrentToken()->Position
                )
            );
        }

        $result = new ConstantExpression($outVal, $targetType);
        $this->_lexer->nextToken();
        return $result;
    }

    /**
     * Parse null literal.
     * 
     * @return ConstantExpression
     */
    private function _parseNullLiteral()
    {
        $this->_lexer->nextToken();
        return new ConstantExpression(null, new Null1());
    }

    /**
     * Check the current token is of a specific kind
     * 
     * @param ExpressionTokenId $expressionTokenId Token to check 
     *                                             with current token.
     * 
     * @return boolean
     */
    private function _tokenIdentifierIs($expressionTokenId)
    {
        return $this->_getCurrentToken()->identifierIs($expressionTokenId);
    }

    /**
     * Validate the current token
     * 
     * @param ExpressionTokenId $expressionTokenId Token to check 
     *                                             with current token.
     * 
     * @return void 
     * 
     * @throws ODataException
     */
    private function _validateToken($expressionTokenId)
    {
        if ($this->_getCurrentToken()->Id != $expressionTokenId) {
            ODataException::createSyntaxError("Syntax error.");
        }
    }

    /**
     * Increment recursion count and throw error if beyond limit
     * 
     * @return void 
     * 
     * @throws ODataException If max recursion limit hits.
     */
    private function _recurseEnter()
    {
        $this->_recursionDepth++;
        if ($this->_recursionDepth == self::RECURSION_LIMIT) {
            ODataException::createSyntaxError("Recursion limit reached.");
        }
    }

    /**
     * Decrement recursion count
     * 
     * @return void
     */
    private function _recurseLeave()
    {
        $this->_recursionDepth--;
    }

    /**
     * Generates Comparison Expression
     * 
     * @param AbstractExpression $left                       The LHS expression.
     * @param AbstractExpression $right                      The RHS expression.
     * @param ExpressionToken    $expressionToken            The cmparision expression token.
     * 
     * @return AbstractExpression
     */
    private static function _generateComparisonExpression($left, $right, $expressionToken)
    {
        switch ($expressionToken->Text) {
            case ODataConstants::KEYWORD_EQUAL:
                return new RelationalExpression(
                    $left, $right, ExpressionType::EQUAL
                );
            case ODataConstants::KEYWORD_NOT_EQUAL:
                return new RelationalExpression(
                    $left, $right, ExpressionType::NOTEQUAL
                );
            case ODataConstants::KEYWORD_GREATERTHAN:
                return new RelationalExpression(
                    $left, $right, ExpressionType::GREATERTHAN
                );
            case ODataConstants::KEYWORD_GREATERTHAN_OR_EQUAL:
                return new RelationalExpression(
                    $left, $right, ExpressionType::GREATERTHAN_OR_EQUAL
                );
            case ODataConstants::KEYWORD_LESSTHAN:
                return new RelationalExpression(
                    $left, $right, ExpressionType::LESSTHAN
                );
            case ODataConstants::KEYWORD_LESSTHAN_OR_EQUAL:
                return new RelationalExpression(
                    $left, $right, ExpressionType::LESSTHAN_OR_EQUAL
                );
            default:
                ODataException::createSyntaxError("Unsupported Comparison Operator: ".$expressionToken->Text);
        }

        
    }
}
?>