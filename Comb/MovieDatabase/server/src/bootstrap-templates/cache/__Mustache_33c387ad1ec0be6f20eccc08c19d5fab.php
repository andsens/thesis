<?php

class __Mustache_33c387ad1ec0be6f20eccc08c19d5fab extends Mustache_Template
{
    public function renderInternal(Mustache_Context $context, $indent = '', $escape = false)
    {
        $buffer = '';

        $buffer .= $indent . '<div class="row-fluid" id="movie-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '">';
        $buffer .= "\n";
        $buffer .= $indent . '	<div class="row-fluid">';
        $buffer .= "\n";
        $buffer .= $indent . '		<div class="span12">';
        $buffer .= "\n";
        $buffer .= $indent . '			<h1>';
        // 'title' inverted section
        $value = $context->find('title');
        if (empty($value)) {
            
            $buffer .= 'Untitled';
        }
        $buffer .= ' ';
        $value = $context->find('title');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= ' <a href="#" class="edit-movie"><i class="icon-edit"></i></a></h1>';
        $buffer .= "\n";
        $buffer .= $indent . '			<h3><span class="year">';
        $value = $context->find('year');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= ' ';
        // 'year' inverted section
        $value = $context->find('year');
        if (empty($value)) {
            
            $buffer .= '0000';
        }
        $buffer .= '</span></h3>';
        $buffer .= "\n";
        $buffer .= $indent . '		</div>';
        $buffer .= "\n";
        $buffer .= $indent . '	</div>';
        $buffer .= "\n";
        $buffer .= $indent . '	<div class="row-fluid">';
        $buffer .= "\n";
        $buffer .= $indent . '		<div class="span12">';
        $buffer .= "\n";
        $buffer .= $indent . '			<div class="accordion">';
        $buffer .= "\n";
        $buffer .= $indent . '			  <div class="accordion-group" id="text-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '">';
        $buffer .= "\n";
        $buffer .= $indent . '			    <div class="accordion-heading">';
        $buffer .= "\n";
        $buffer .= $indent . '			      <a class="accordion-toggle" data-toggle="collapse" data-parent="#text-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '" href="#synopsis-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '">';
        $buffer .= "\n";
        $buffer .= $indent . '			      	Synopsis';
        $buffer .= "\n";
        $buffer .= $indent . '			      </a>';
        $buffer .= "\n";
        $buffer .= $indent . '			    </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			    <div id="synopsis-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '" class="accordion-body collapse in">';
        $buffer .= "\n";
        $buffer .= $indent . '			      <div class="accordion-inner">';
        $value = $context->find('synopsis');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '</div>';
        $buffer .= "\n";
        $buffer .= $indent . '			    </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			  </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			  <div class="accordion-group">';
        $buffer .= "\n";
        $buffer .= $indent . '			    <div class="accordion-heading">';
        $buffer .= "\n";
        $buffer .= $indent . '			      <a class="accordion-toggle" data-toggle="collapse" data-parent="#text-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '" href="#plot-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '">';
        $buffer .= "\n";
        $buffer .= $indent . '			        Plot';
        $buffer .= "\n";
        $buffer .= $indent . '			      </a>';
        $buffer .= "\n";
        $buffer .= $indent . '			    </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			    <div id="plot-';
        $value = $context->find('id');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= '" class="accordion-body collapse">';
        $buffer .= "\n";
        $buffer .= $indent . '			      <div class="accordion-inner">';
        $buffer .= "\n";
        $buffer .= $indent . '			        ';
        $value = $context->find('plot');
        if (!is_string($value) && is_callable($value)) {
            $value = $this->mustache
                ->loadLambda((string) call_user_func($value))
                ->renderInternal($context, $indent);
        }
        $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
        $buffer .= "\n";
        $buffer .= $indent . '			      </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			    </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			  </div>';
        $buffer .= "\n";
        $buffer .= $indent . '			</div>';
        $buffer .= "\n";
        $buffer .= $indent . '		</div>';
        $buffer .= "\n";
        $buffer .= $indent . '	</div>';
        $buffer .= "\n";
        $buffer .= $indent . '	<div class="row-fluid">';
        $buffer .= "\n";
        $buffer .= $indent . '		<div class="span4">';
        $buffer .= "\n";
        $buffer .= $indent . '			<table class="cast table">';
        $buffer .= "\n";
        $buffer .= $indent . '				<thead>';
        $buffer .= "\n";
        $buffer .= $indent . '					<tr>';
        $buffer .= "\n";
        $buffer .= $indent . '						<td>Actor</td>';
        $buffer .= "\n";
        $buffer .= $indent . '						<td>Character <a href="#" class="add-role"><i class="icon-plus"></i></a></td>';
        $buffer .= "\n";
        $buffer .= $indent . '					</tr>';
        $buffer .= "\n";
        $buffer .= $indent . '				</thead>';
        $buffer .= "\n";
        $buffer .= $indent . '				<tbody>';
        $buffer .= "\n";
        $buffer .= $indent . '					';
        // 'cast' section
        $buffer .= $this->section1094881497f4f310212e4ca00997f358($context, $indent, $context->find('cast'));
        $buffer .= "\n";
        $buffer .= $indent . '				</tbody>';
        $buffer .= "\n";
        $buffer .= $indent . '			</table>';
        $buffer .= "\n";
        $buffer .= $indent . '		</div>';
        $buffer .= "\n";
        $buffer .= $indent . '	</div>';
        $buffer .= "\n";
        $buffer .= $indent . '</div>';
        $buffer .= "\n";

        if ($escape) {
            return htmlspecialchars($buffer, ENT_COMPAT, 'UTF-8');
        } else {
            return $buffer;
        }
    }

    private function section1094881497f4f310212e4ca00997f358(Mustache_Context $context, $indent, $value) {
        $buffer = '';
        if (!is_string($value) && is_callable($value)) {
            $source = '<tr id="role-{{id}}">{{>role}}</tr>';
            $buffer .= $this->mustache
                ->loadLambda((string) call_user_func($value, $source))
                ->renderInternal($context, $indent);
        } elseif (!empty($value)) {
            $values = $this->isIterable($value) ? $value : array($value);
            foreach ($values as $value) {
                $context->push($value);
                $buffer .= '<tr id="role-';
                $value = $context->find('id');
                if (!is_string($value) && is_callable($value)) {
                    $value = $this->mustache
                        ->loadLambda((string) call_user_func($value))
                        ->renderInternal($context, $indent);
                }
                $buffer .= htmlspecialchars($value, ENT_COMPAT, 'UTF-8');
                $buffer .= '">';
                if ($partial = $this->mustache->loadPartial('role')) {
                    $buffer .= $partial->renderInternal($context, '');
                }
                $buffer .= '</tr>';
                $context->pop();
            }
        }
    
        return $buffer;
    }
}