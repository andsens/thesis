<?php

class __Mustache_faf1d1d4f45baf8ab951ba703dfb4c02 extends Mustache_Template
{
    public function renderInternal(Mustache_Context $context, $indent = '', $escape = false)
    {
        $buffer = '';

        // 'actor' section
        $buffer .= $this->section35aa17d0378b4c98ba635a0877082b28($context, $indent, $context->find('actor'));
        $buffer .= "\n";
        $buffer .= $indent . '<td class="character">';
        // 'character' inverted section
        $value = $context->find('character');
        if (empty($value)) {
            
            $buffer .= 'Unnamed';
        }
        $buffer .= ' ';
        $value = $context->find('character');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '</td>';
        $buffer .= "\n";

        if ($escape) {
            return htmlspecialchars($buffer, ENT_COMPAT, 'UTF-8');
        } else {
            return $buffer;
        }
    }

    private function section35aa17d0378b4c98ba635a0877082b28(Mustache_Context $context, $indent, $value) {
        $buffer = '';
        if (!is_string($value) && is_callable($value)) {
            $source = '<td class="actor">{{>actor}}</td>';
            $buffer .= $this->mustache
                ->loadLambda((string) call_user_func($value, $source))
                ->renderInternal($context, $indent);
        } elseif (!empty($value)) {
            $values = $this->isIterable($value) ? $value : array($value);
            foreach ($values as $value) {
                $context->push($value);
                $buffer .= $indent . '<td class="actor">';
                if ($partial = $this->mustache->loadPartial('actor')) {
                    $buffer .= $partial->renderInternal($context, '');
                }
                $buffer .= '</td>';
                $context->pop();
            }
        }
    
        return $buffer;
    }
}