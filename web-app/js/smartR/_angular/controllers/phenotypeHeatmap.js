//# sourceURL=phenotypeHeatmap.js

'use strict';

window.smartRApp.controller('PhenotypeHeatmapController', [
    '$scope',
    'commonWorkflowService',
    'smartRUtils',
    function($scope, commonWorkflowService, smartRUtils) {

        commonWorkflowService.initializeWorkflow('phenotypeHeatmap', $scope);

        // ------------------------------------------------------------- //
        // Fetch data                                                    //
        // ------------------------------------------------------------- //
        $scope.fetch = {
            disabled: false,
            running: false,
            loaded: false,
            conceptBoxes: {
                numeric: {concepts: [], valid: false},
                column: {concepts: [], valid: false},
                row: {concepts: [], valid: false}
            }
        };

        // ------------------------------------------------------------- //
        // Run Heatmap                                                   //
        // ------------------------------------------------------------- //
        $scope.runAnalysis = {
            disabled: true,
            running: false,
            params: {
                selections: {
                    selectedRownames: [],
                },
                max_row: 100,
                sorting: 'patientnumbers',
                ranking: 'mean',
                binnedRow: {
                	active: false,
                	procentual: false,
                    start: 0,
                    end: 100,
                    step: 10
                },
                binnedColumn: {
                	active: false,
                	procentual: false,
                    start: 0,
                    end: 100,
                    step: 10
                }
            },
            download: {
                disabled: true
            },
            scriptResults: {}
        };

        $scope.common = {
            totalSamples: 0,
            numberOfRows: 0,
            subsets: 0
        };

        $scope.$watchGroup(['fetch.running', 'runAnalysis.running'], function(newValues) {
            var fetchRunning = newValues[0],
                runAnalysisRunning = newValues[1];

            // clear old results
            if (fetchRunning) {
                $scope.runAnalysis.scriptResults = {};
                $scope.runAnalysis.params.ranking = '';
                $scope.common.subsets = smartRUtils.countCohorts();
            }

            // disable tabs when certain criteria are not met
            $scope.fetch.disabled = runAnalysisRunning;
            $scope.runAnalysis.disabled = fetchRunning || !$scope.fetch.loaded;

            // disable buttons when certain criteria are not met
            $scope.runAnalysis.download.disabled = runAnalysisRunning ||
                $.isEmptyObject($scope.runAnalysis.scriptResults);

            // load binning parameters from fetching phase to runAnalysis.params
            if (!fetchRunning) {
            	$scope.runAnalysis.params.binnedRow = $scope.fetch.conceptBoxes.row.binning;
            	$scope.runAnalysis.params.binnedColumn = $scope.fetch.conceptBoxes.column.binning;
            }
            
            // set ranking criteria
            if (!fetchRunning &&
                $scope.common.totalSamples < 2 &&
                $scope.runAnalysis.params.ranking === '') {
                $scope.runAnalysis.params.ranking = 'mean';
            } else if (!fetchRunning &&
                       $scope.common.subsets < 2 &&
                       $scope.runAnalysis.params.ranking === '') {
                $scope.runAnalysis.params.ranking = 'mean';
            } else if (!fetchRunning &&
                       $scope.common.subsets > 1 &&
                       $scope.runAnalysis.params.ranking === '') {
                $scope.runAnalysis.params.ranking = 'adjpval';
            }
        });
    }]);
