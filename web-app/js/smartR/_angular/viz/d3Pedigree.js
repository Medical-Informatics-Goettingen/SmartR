//# sourceURL=d3Pedigree.js

'use strict';

window.smartRApp.directive('pedigree', [
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
            templateUrl: $rootScope.smartRPath +  '/js/smartR/_angular/templates/pedigree.html',
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
                        createPedigree(scope, template_viz, template_ctrl);
                    }
                });
            }
        };


        function createPedigree(scope, vizDiv) {

        // <img style="height=10%" src="/tmp/Rserv/conn22738/Bild1.png"
        // class="img-result-size_sysinflame"/>
            var width = parseInt(scope.width);
            var height = parseInt(scope.height);
            var margin = {top: 20, right: 60, bottom: 200, left: 280};

            var svg = d3.select(vizDiv).append("svg").attr("width",
                width + margin.left + margin.right).attr("height",
                height + margin.top + margin.bottom).append("g").attr(
                "transform",
                "translate(" + margin.left + "," + margin.top + ")");

svg.append('img')
    .attr('class', 'picture')
    .attr('src', "data:image/png;base64," + scope.data.plots[0]);


            // for (var i = 0; i < scope.data.plots.length ; i++) {
            //     document.getElementById("ped").src = "data:image/png;base64," + scope.data.plots[0];
            //
            // }
            console.log(scope)
            console.log(scope.data);

        }


    }]);

