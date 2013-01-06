<?php

class __Mustache_dd5b395400271ec1d473fcc5c75a4b74 extends Mustache_Template
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
        $buffer .= $indent . '		';
        $buffer .= "\n";
        $buffer .= $indent . '		<link rel="stylesheet" href="styles/bootstrap.min.css"/>';
        $buffer .= "\n";
        $buffer .= $indent . '	</head>';
        $buffer .= "\n";
        $buffer .= $indent . '	<body>';
        $buffer .= "\n";
        $buffer .= $indent . '    <div class="container-fluid" id="movies">';
        $buffer .= "\n";
        $buffer .= $indent . '    	<div class="row-fluid">';
        $buffer .= "\n";
        $buffer .= $indent . '				<div class="span2">';
        $buffer .= "\n";
        $buffer .= $indent . '					<ul class="nav nav-list">';
        $buffer .= "\n";
        $buffer .= $indent . '						<li class="nav-header">Movies</li>';
        $buffer .= "\n";
        $buffer .= $indent . '						';
        // 'movies' section
        $buffer .= $this->section0453b22be55f1b169f53d91bbe82e092($context, $indent, $context->find('movies'));
        $buffer .= "\n";
        $buffer .= $indent . '						<li class="nav-header"><a href="#" id="add-movie"><i class="icon-plus"></i></a></li>';
        $buffer .= "\n";
        $buffer .= $indent . '					</ul>';
        $buffer .= "\n";
        $buffer .= $indent . '				</div>';
        $buffer .= "\n";
        $buffer .= $indent . '				<div class="span10">';
        $buffer .= "\n";
        $buffer .= $indent . '					<div class="tab-content movie-list">';
        $buffer .= "\n";
        $buffer .= $indent . '						';
        // 'movies' section
        $buffer .= $this->sectionE48f9c865788f608bc9ae9a08385d70f($context, $indent, $context->find('movies'));
        $buffer .= "\n";
        $buffer .= $indent . '					</div>';
        $buffer .= "\n";
        $buffer .= $indent . '				</div>';
        $buffer .= "\n";
        $buffer .= $indent . '			</div>';
        $buffer .= "\n";
        $buffer .= $indent . '		</div>';
        $buffer .= "\n";
        $buffer .= $indent . '		<footer></footer>';
        $buffer .= "\n";
        $buffer .= $indent . '	<script data-main="scripts/main" src="scripts/vendor/require.js"></script>';
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

    private function section0453b22be55f1b169f53d91bbe82e092(Mustache_Context $context, $indent, $value) {
        $buffer = '';
        if (!is_string($value) && is_callable($value)) {
            $source = '<li><a data-toggle="tab" href="#movietab-{{id}}">{{title}}</a></li>';
            $buffer .= $this->mustache
                ->loadLambda((string) call_user_func($value, $source))
                ->renderInternal($context, $indent);
        } elseif (!empty($value)) {
            $values = $this->isIterable($value) ? $value : array($value);
            foreach ($values as $value) {
                $context->push($value);
                $buffer .= '<li><a data-toggle="tab" href="#movietab-';
                $value = $context->find('id');
                if (!is_string($value) && is_callable($value)) {
                    $value = $this->mustache
                        ->loadLambda((string) call_user_func($value))
                        ->renderInternal($context, $indent);
                }
                $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
                $buffer .= '">';
                $value = $context->find('title');
                if (!is_string($value) && is_callable($value)) {
                    $value = $this->mustache
                        ->loadLambda((string) call_user_func($value))
                        ->renderInternal($context, $indent);
                }
                $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
                $buffer .= '</a></li>';
                $context->pop();
            }
        }
    
        return $buffer;
    }

    private function sectionE48f9c865788f608bc9ae9a08385d70f(Mustache_Context $context, $indent, $value) {
        $buffer = '';
        if (!is_string($value) && is_callable($value)) {
            $source = '<div class="tab-pane" id="movietab-{{id}}">{{>movie}}</div>';
            $buffer .= $this->mustache
                ->loadLambda((string) call_user_func($value, $source))
                ->renderInternal($context, $indent);
        } elseif (!empty($value)) {
            $values = $this->isIterable($value) ? $value : array($value);
            foreach ($values as $value) {
                $context->push($value);
                $buffer .= '<div class="tab-pane" id="movietab-';
                $value = $context->find('id');
                if (!is_string($value) && is_callable($value)) {
                    $value = $this->mustache
                        ->loadLambda((string) call_user_func($value))
                        ->renderInternal($context, $indent);
                }
                $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
                $buffer .= '">';
                if ($partial = $this->mustache->loadPartial('movie')) {
                    $buffer .= $partial->renderInternal($context, '');
                }
                $buffer .= '</div>';
                $context->pop();
            }
        }
    
        return $buffer;
    }
}