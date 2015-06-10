<?php
/** 
 * Expression class specialized for a property access.
 * 
 * PHP version 5.3
 * 
 * @category  ODataPHPProd
 * @package   ODataProducer_UriProcessor_QueryProcessor_ExpressionParser_Expressions
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
namespace ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions;
use ODataProducer\UriProcessor\QueryProcessor\FunctionDescription\FunctionDescription;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\LogicalExpression;
use ODataProducer\UriProcessor\QueryProcessor\ExpressionParser\Expressions\AbstractExpression;
use ODataProducer\Providers\Metadata\ResourceTypeKind;
use ODataProducer\Providers\Metadata\ResourceProperty;
use ODataProducer\Providers\Metadata\Type\Navigation;
use ODataProducer\Providers\Metadata\Type\Boolean;
/**
 * Expression class for property access.
 *
 * @category  ODataPHPProd
 * @package   ODataProducer_UriProcessor_QueryProcessor_ExpressionParser_Expressions
 * @author    Microsoft Open Technologies, Inc. <msopentech@microsoft.com>
 * @copyright Microsoft Open Technologies, Inc.
 * @license   New BSD license, (http://www.opensource.org/licenses/bsd-license.php)
 * @version   GIT: 1.2
 * @link      https://github.com/MSOpenTech/odataphpprod
 */
class SimplePropertyAccessExpression extends AbstractExpression
{
    /**
     * @var PropertyAccessExpression
     */
    protected $parent;

    /**
     * @var PropertyAccessExpression
     */
    protected $child;

    /**
     * @var string
     */
    protected $property;

    /**
     * Creates new instance of PropertyAccessExpression
     * 
     * @param PropertyAccessExpression $parent           The parent expression
     * @param string         $identifier 
     */
    public function __construct($parent, $property)
    {
        $this->parent = $parent;
        $this->child = null;
        $this->nodeType = ExpressionType::PROPERTYACCESS;
        $this->property = $property;

        $this->type = new \ODataProducer\Providers\Metadata\Type\String();

        if (!is_null($parent)) {
            $parent->setChild($this);
        }
    }

    /**
     * To set the child if any
     * 
     * @param PropertyAccessExpression $child The child expression
     * 
     * @return void
     */
    public function setChild($child)
    {
        $this->child = $child;
    }

    /**
     * To get the parent. If this property is property of entity 
     * then return null, If this property is property of complex type 
     * then return PropertyAccessExpression for the parent complex type
     * 
     * @return PropertyAccessExpression
     */
    public function getParent()
    {
        return $this->parent;
    }

    /**
     * To get the child. Returns null if no child property
     * 
     * @return PropertyAccessExpression
     */
    public function getChild()
    {
        return $this->child;
    }

    /**
     * To get the property name
     * 
     * @return string
     */
    public function getPropertyName()
    {
        return $this->property;
    }
    
    /**
     * (non-PHPdoc)
     * 
     * @see library/ODataProducer/QueryProcessor/Expressions/ODataProducer\QueryProcessor\Expressions.AbstractExpression::free()
     * 
     * @return void
     */
    public function free()
    {
        if (!is_null($this->parent)) {
            $this->parent->free();
            unset($this->parent);
        }
        
        if (!is_null($this->child)) {
            $this->child->free();
            unset($this->child);
        }
    }
}
?>