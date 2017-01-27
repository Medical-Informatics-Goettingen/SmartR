//# sourceURL=choiceConceptBoxMiGoe.js

'use strict';

window.smartRApp.directive('choiceConceptBoxMiGoe', [
    '$rootScope',
    '$http',
    function($rootScope, $http) {
    	
    	$.widget("ui.suffixSpinner", $.ui.spinner, {
    		_format: function(value) {
    			var suffix = this.options.suffix;
    			return value+" "+suffix;
    		},
			_parse: function(value) {
				return parseFloat(value);
			}
    	});
    	
    	$rootScope.radioBtChange = function(value) {
    		alert(value);
    	}
    	
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
                
                $('.choiceSliderValueMiGoe').suffixSpinner({
                	step: 0.1,
                	min: 0.1,
                	max: 100.0,
                	start: 10.0,
                	suffix: "%",
                	numberFormat: "n",
            		change: function(event, ui) {
            			if (!event.originalEvent) return;
            			if (event.originalEvent.type !== "blur") return;
            			$('.choiceSliderValueMiGoe').suffixSpinner("stepUp");
            			$('.choiceSliderValueMiGoe').suffixSpinner("stepDown");
            		}
                });
                
                $('.choiceConceptBoxSliderMiGoe').slider({
                	range: "min",
                	value: 100,
                	min: 1,
                	max: 1000,
                	slide: function(event, ui) {
                		this.parentElement.querySelector('.choiceSliderValueMiGoe').innerHTML = ui.value/10 + " %";
                	}
                });
                
                var isChoice2 = function() {
                	return template_choice2.classList.contains("ng-valid-parse");
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
                    dtgI.notifyDrop = dropOntoCategorySelection; // jshint ignore:line
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
                    scope.instructionNodeType = !_containsOnlyCorrectType();
                    // FIXME: Disabled for now because this causes problems with certain datasets for unknown reasons
                    // if (scope.type === 'HD' && scope.conceptGroup.concepts.length > 1) {
                    //     _getNodeDetails(scope.conceptGroup.concepts, function(response) {
                    //         if (Object.keys(response.data).length < 2) {
                    //             var platforms = response.data[Object.keys(response.data)[0]].platforms;
                    //             scope.instructionNodePlatform = !platforms.every(function(el) { 
                    //                 return el.title === platforms[0].title;
                    //             });
                    //         } else {
                    //             scope.instructionNodePlatform = true;
                    //         }
                    //     });
                    // } else {
                    //     scope.instructionNodePlatform = false;
                    // }
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
                
                scope.$watch('fetchData.choice', function(value) {
                	if (value === undefined) return;
                	if (value == "choice1") {
                		document.getElementById(scope.identification).querySelector('.sliderContainerMiGoe').style.display = "";
                		document.getElementById(scope.identification).querySelector('.noSliderContainerMiGoe').style.marginBottom = "";
                	} else if (value == "choice2") {
                		document.getElementById(scope.identification).querySelector('.sliderContainerMiGoe').style.display = "block";
                		document.getElementById(scope.identification).querySelector('.noSliderContainerMiGoe').style.marginBottom = "0";
                	}
                });

                scope.validate();
            }
        };
    }]);
