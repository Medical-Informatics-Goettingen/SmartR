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
                var vizDiv = element.children()[0];
                /**
                 * Watch data model (which is only changed by ajax calls when we want to (re)draw everything)
                 */
                scope.$watch('data', function () {
                    $(vizDiv).empty();
                    if (! $.isEmptyObject(scope.data)) {
                        createBoxplot2(scope, vizDiv);
                    }
                });
            }
        };

        function createBoxplot2(scope, vizDiv) {

            var scope = scope.data;
            console.log(scope);
            var mode = scope.mode;
            console.log("mode: "  +mode);

            var subsets = scope.subset;
            console.log("subsets: " + subsets)

            var plotData = [];

            subsets.forEach(function(f){
                console.log("subset: " + f)
                var data;
                if (f==1)
                    data = scope.data.s1;
                else
                    data = scope.data.s2;
                var xData = [];
                var yData = [];
                //subset1 & 2
                data.forEach(function(x){

                    if (yData[x.meta]) {
                        yData[x.meta].push(x.alpha);
                    }
                    else {
                        yData[x.meta] = [];
                        xData.push(x.meta);
                    }
                });


                console.log("yData")
                console.log(yData)
                console.log("xData")
                console.log(xData)
                //subsets

                for (var i = 0; i < xData.length; i++) {
                    console.log("xData " + xData[i]);
                    console.log("yData " + yData[xData[i]])
                    plotData.push({
                        type: 'box',
                        y: yData[xData[i]],
                        name: "Subset: " + f + "<br>Category: " + xData[i],
                        boxpoints: 'all',
                        boxmean: 'sd',
                        jitter: 0.5
                    });
                }
            });
            for (var subset = 1; subset<=subsets; subset++) {

            }
            console.log("title: " + scope.mode);
            var layout = {
                title: 'Alpha-Diversity (' + mode + ')',
                height: 800
            };

            // vizDiv = document.getElementById('test');

            Plotly.newPlot(vizDiv, plotData, layout);

        }

    }]);

