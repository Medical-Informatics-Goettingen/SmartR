//# sourceURL=choiceConceptBoxMiGoe.js

'use strict';

window.smartRApp.directive('choiceConceptBoxMiGoe', [
    '$rootScope',
    '$http',
    function($rootScope, $http) {
    	
        return {
            restrict: 'E',
            scope: {
                conceptGroup: '=',
                identification: '@',
                label: '@',
                tooltip: '@',
                min1: '@',
                max1: '@',
                min2: '@',
                max2: '@',
                type1: '@',
                type2: '@',
                choice1: '@',
                choice2: '@'
            },
            templateUrl: $rootScope.smartRPath +  '/js/smartR/_angular/templates/choiceConceptBoxMiGoe.html',
            link: function(scope, element) {
                var max1 = parseInt(scope.max1);
                var min1 = parseInt(scope.min1);
                var max2 = parseInt(scope.max2);
                var min2 = parseInt(scope.min2);
                
                var template_box = element[0].querySelector('.sr-drop-input'),
                    template_btn = element[0].querySelector('.sr-drop-btn'),
                    template_tooltip = element[0].querySelector('.sr-tooltip-dialog'),
                    template_choice1 = element[0].querySelector('.sr-choice1-mi-goe'),
                    template_choice2 = element[0].querySelector('.sr-choice2-mi-goe');
                
                scope.conceptGroup.binning = {
                		active: false,
                		procentual: false,
                		start: 0,
                		end: 100,
                		step: 10
                };
                
                template_choice1.addEventListener("click", function() {
                	scope.conceptGroup.binning.active = false;
            		document.getElementById(scope.identification).querySelector('.sliderContainerMiGoe').style.display = "";
            		document.getElementById(scope.identification).querySelector('.noSliderContainerMiGoe').style.marginBottom = "";
            		
            		template_choice1.classList.add("buttonInUsage");
            		template_choice2.classList.remove("buttonInUsage");
            		
            		scope.validate();
                })
                
                template_choice2.addEventListener("click", function() {
                	scope.conceptGroup.binning.active = true;
            		document.getElementById(scope.identification).querySelector('.sliderContainerMiGoe').style.display = "block";
            		document.getElementById(scope.identification).querySelector('.noSliderContainerMiGoe').style.marginBottom = "0";
            		
            		template_choice1.classList.remove("buttonInUsage");
            		template_choice2.classList.add("buttonInUsage");
            		
            		scope.validate();
                })
                
                var template_procentualBinning = element[0].querySelector('.sr-procentualBinned-mi-goe');
                
                template_procentualBinning.addEventListener("click", function() {
                	scope.conceptGroup.binning.procentual = !scope.conceptGroup.binning.procentual;
                	
                	template_procentualBinning.classList.toggle("buttonInUsage");

                	scope.validate();
                })
                
                
                var startSpinner = $('.choiceStartValueMiGoe');
                startSpinner.spinner({
                	start: 0,
                	numberFormat: "n"
                });
               
                var endSpinner = $('.choiceEndValueMiGoe');
                endSpinner.spinner({
                	start: 100,
                	numberFormat: "n"
                })
                
                var stepSpinner = $('.choiceStepValueMiGoe');
                stepSpinner.spinner({
                	start: 10,
                	numberFormat: "n"
                })
                        
                var spinnerChange = function(event, ui) {
                	var newValue = isNaN(parseFloat(this.value)) ? this.getAttribute("aria-valuenow") : parseFloat(this.value);
                	$(this).spinner('value', newValue);
                };
                
                startSpinner.on("change", function(event, ui) {
                	if (this.value === "MIN") $(this).spinner('value', this.value);
                	else spinnerChange(event, ui);
                	scope.conceptGroup.binning.start = $(this).spinner('value');
                	scope.validate();
                });
                
                endSpinner.on("change", function(event, ui) {
                	if (this.value === "MAX") $(this).spinner('value', this.value);
                	else spinnerChange(event, ui);
                	scope.conceptGroup.binning.end = $(this).spinner('value');
                	scope.validate();
                });
                
                stepSpinner.on("change", function(event, ui) {
                	spinnerChange();
                	scope.conceptGroup.binning.step = $(this).spinner('value');
                	scope.validate();
                });
                
                var isChoice2 = function() {
                	return scope.conceptGroup.binning.active;
                }
                
                // instantiate tooltips
                $(template_tooltip).tooltip({track: true, tooltipClass:"sr-ui-tooltip"});

                var _clearWindow = function() {
                    $(template_box).children().remove();
                };

                var _getConcepts = function() {
                    return $(template_box).children().toArray().map(function(childNode) {
                        return childNode.getAttribute('conceptid');
                    });
                };

                var _activateDragAndDrop = function() {
                    var extObj = Ext.get(template_box);
                    var dtgI = new Ext.dd.DropTarget(extObj, {ddGroup: 'makeQuery'});
                    dtgI.notifyDrop = dropWithFolderOntoSelection;
                    
                    function dropWithFolderOntoSelection(source, e, data)
                    {
                        var targetdiv=this.el;
                        if(data.node.leaf == false && !data.node.isLoaded()) {
                            data.node.reload(function(){
                                analysisConcept = dropWithFolderOntoSelection2(source, e, data, targetdiv);
                            });
                        }
                        else {
                            analysisConcept = dropWithFolderOntoSelection2(source, e, data, targetdiv);
                        }
                        return true;
                    }

                    function dropWithFolderOntoSelection2(source, e, data, targetdiv)
                    {
                        var concept = createPanelItemNew(targetdiv, convertNodeToConcept(data.node));
                        return concept;
                    }  
                };

                var typeMap = {
                    hleaficon: 'HD',
                    alphaicon: 'LD-categorical',
                    null: 'LD-categorical', // a fix for older tm version without alphaicon
                    valueicon: 'LD-numerical'
                };
                var _containsOnlyCorrectType = function() {
                    return $(template_box).children().toArray().every(function(childNode) {
                    	if (isChoice2()) return typeMap[childNode.getAttribute('setnodetype')] === scope.type2;
                    	return typeMap[childNode.getAttribute('setnodetype')] === scope.type1;
                    });
                };

                var _getNodeDetails = function(conceptKeys, callback) {
                    var request = $http({
                        url: pageInfo.basePath + '/SmartR/nodeDetails',
                        method: 'POST',
                        config: {
                            timeout: 10000
                        },
                        data: {
                            conceptKeys: conceptKeys
                        }
                    });

                    request.then(
                        callback,
                        function() {
                            alert('Could not fetch node details. Network connection lost?');
                        });
                };

                // activate drag & drop for our conceptBox and color it once it is rendered
                scope.$evalAsync(function() {
                    _activateDragAndDrop();
                });

                // bind the button to its clearing functionality
                template_btn.addEventListener('click', function() {
                    _clearWindow();
                });

                // this watches the childNodes of the conceptBox and updates the model on change
                new MutationObserver(function() {
                	scope.conceptGroup.concepts = _getConcepts(); // update the model
                	/*scope.conceptGroup.binning = {
                			active: isChoice2(),
                			procentual: template_procentualBinning.checked,
                			start: startSpinner.spinner('value'),
                			end: endSpinner.spinner('value'),
                			step: stepSpinner.spinner('value')
                	}*/
                    scope.validate();
                    scope.$apply();
                }).observe(template_box, { childList: true });

                scope.validate = function() {
                	var min, max;
                	if (isChoice2()) {
                		min = min2;
                		max = max2;
                	} else {
                		min = min1;
                		max = max1;
                	}
                    scope.instructionMinNodes = scope.conceptGroup.concepts.length < min;
                    scope.instructionMaxNodes = max !== -1 && scope.conceptGroup.concepts.length > max;

                    element[0].querySelector('.instructionMinNodes').innerHTML = "Drag at least " + min + " node(s) into the box<br/>";
                    element[0].querySelector('.instructionMaxNodes').innerHTML = "Select at most " + max + " node(s)<br/>";
                    
                    scope.instructionNodeType = !_containsOnlyCorrectType();
                    scope.instructionNodePlatform = false;
                };

                scope.$watchGroup([
                    'instructionNodeType',
                    'instructionNodePlatform',
                    'instructionMaxNodes',
                    'instructionMinNodes'],
                    function(newValue) {
                        var instructionNodeType = newValue[0],
                            instructionNodePlatform = newValue[1],
                            instructionMaxNodes = newValue[2],
                            instructionMinNodes = newValue[3];
                        scope.conceptGroup.valid = !(instructionNodeType ||
                                                     instructionNodePlatform ||
                                                     instructionMaxNodes ||
                                                     instructionMinNodes);
                    });

                scope.validate();
            }
        };
    }]);
