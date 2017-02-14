//# sourceURL=d3Alphadiv.js

'use strict';

window.smartRApp.directive('alphadiv', [
    'smartRUtils',
    'rServeService',
    '$rootScope',
    function(smartRUtils, rServeService, $rootScope) {

        return {
            restrict: 'E',
            scope: {
                data: '=',
                width: '@',
                height: '@'
            },
            templateUrl: $rootScope.smartRPath +  '/js/smartR/_angular/templates/alphadiv.html',
            link: function (scope, element) {
                var template_ctrl = element.children()[0],
                    template_viz = element.children()[1];
                /**
                 * Watch data model (which is only changed by ajax calls when we want to (re)draw everything)
                 */
                scope.$watch('data', function () {
                    $(template_viz).empty();
                    if (! $.isEmptyObject(scope.data)) {
                        smartRUtils.prepareWindowSize(scope.width, scope.height);
                        scope.showControls = true;
                        createBoxplot2(scope, template_viz, template_ctrl);
                    }
                });
            }
        };


        function createBoxplot2(scope, vizDiv) {
            console.log("test2")
            console.log(scope)

            console.log(scope.data);
            console.log("mode: "  +scope.data.mode);

            var data = scope.data.data;

            console.log("data");
            console.log(data);


            data.forEach(function(x){
                console.log(x.alpha);
            });

            var cf = crossfilter(data);
            console.log("boxplot data")
            console.log(scope.data)
            console.log("boxplot data")
            var byValue = cf.dimension(function(d) { return d.alpha; });
            var bySubset = cf.dimension(function(d) { return d.subset; });
            var byBioMarker = cf.dimension(function(d) { return d.meta; });

            var plotData = [];
            smartRUtils.unique(smartRUtils.getValuesForDimension(byBioMarker)).forEach(function(bioMarker) {
                byBioMarker.filterExact(bioMarker);
                smartRUtils.unique(smartRUtils.getValuesForDimension(bySubset, true)).forEach(function(subset) {
                    bySubset.filterExact(subset);
                    plotData.push({
                        type: 'box',
                        y: smartRUtils.getValuesForDimension(byValue),
                        name: bioMarker + ' s' + subset,
                        boxpoints: 'all',
                        boxmean: 'sd',
                        jitter: 0.5
                    });
                    bySubset.filterAll();
                });
                byBioMarker.filterAll();
            });

            var layout = {
                title: 'Boxplots (' + scope.data.transformation + ')',
                height: 800
            };
            Plotly.newPlot(vizDiv, plotData, layout);





        }


    }]);

