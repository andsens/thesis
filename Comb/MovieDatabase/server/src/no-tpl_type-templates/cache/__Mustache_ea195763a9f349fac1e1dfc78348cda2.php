<?php

class __Mustache_ea195763a9f349fac1e1dfc78348cda2 extends Mustache_Template
{
    public function renderInternal(Mustache_Context $context, $indent = '', $escape = false)
    {
        $buffer = '';

        $buffer .= $indent . '<!DOCTYPE html>';
        $buffer .= "\n";
        $buffer .= $indent . '<!--[if lt IE 7]>      <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->';
        $buffer .= "\n";
        $buffer .= $indent . '<!--[if IE 7]>         <html class="no-js lt-ie9 lt-ie8"> <![endif]-->';
        $buffer .= "\n";
        $buffer .= $indent . '<!--[if IE 8]>         <html class="no-js lt-ie9"> <![endif]-->';
        $buffer .= "\n";
        $buffer .= $indent . '<!--[if gt IE 8]><!--> <html class="no-js"> <!--<![endif]-->';
        $buffer .= "\n";
        $buffer .= $indent . '	<head>';
        $buffer .= "\n";
        $buffer .= $indent . '		<meta charset="utf-8"/>';
        $buffer .= "\n";
        $buffer .= $indent . '		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1"/>';
        $buffer .= "\n";
        $buffer .= $indent . '		<title></title>';
        $buffer .= "\n";
        $buffer .= $indent . '		<meta name="description" content=""/>';
        $buffer .= "\n";
        $buffer .= $indent . '		<meta name="viewport" content="width=device-width"/>';
        $buffer .= "\n";
        $buffer .= $indent . '	</head>';
        $buffer .= "\n";
        $buffer .= $indent . '	<body>';
        $buffer .= "\n";
        $buffer .= $indent . '		<a href="no-bootstrap">Without bootstrap templates</a>';
        $buffer .= "\n";
        $buffer .= $indent . '		<a href="bootstrap">With bootstrap templates</a>';
        $buffer .= "\n";
        $buffer .= $indent . '</body>';
        $buffer .= "\n";
        $buffer .= $indent . '</html>';
        $buffer .= "\n";

        if ($escape) {
            return htmlspecialchars($buffer, ENT_COMPAT, 'UTF-8');
        } else {
            return $buffer;
        }
    }

}