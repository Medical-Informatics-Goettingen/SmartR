<script type="text/ng-template" id="phenotypeHeatmap">
<div ng-controller="PhenotypeHeatmapController">

    <tab-container>
        %{--========================================================================================================--}%
        %{-- Fetch Data --}%
        %{--========================================================================================================--}%
        <workflow-tab tab-name="Fetch Data" disabled="fetch.disabled">
            <concept-box
                concept-group="fetch.conceptBoxes.categoric"
                type="LD-categorical"
                min="1"
                max="40"
                label="Categoric Row Variable"
                tooltip="Insert any amount of concepts from ONE category in here. The row variables will be used to calculate the significane values.">
            </concept-box>

			<concept-box
                concept-group="fetch.conceptBoxes.categoric2"
                type="LD-categorical"
                min="1"
                max="40"
                label="Categoric Column Variable"
                tooltip="Insert any amount of concept from ONE category in here. Do not use the same category as used for the Categoric Row Variable.">
            </concept-box>

			<concept-box
                concept-group="fetch.conceptBoxes.numeric"
                type="LD-numerical"
                min="0"
                max="1"
                label="Numerical Variables (optional)"
                tooltip="Select one numerical variable if you want to use this variable instead of the patient count for sorting.">
            </concept-box>

            <hr class="sr-divider">
            <fetch-button
                    loaded="fetch.loaded"
                    running="fetch.running"
                    concept-map="fetch.conceptBoxes"
                    all-samples="common.totalSamples"
                    allowed-cohorts="[1]"
                    number-of-rows="common.numberOfRows">
            </fetch-button>
        </workflow-tab>

        %{--========================================================================================================--}%
        %{--Run Analysis--}%
        %{--========================================================================================================--}%
        <workflow-tab tab-name="Run Analysis" disabled="runAnalysis.disabled">
            %{--Number of max row to display--}%
            <div class="heim-input-field heim-input-number sr-input-area">
                Show <input type="text" id="txtMaxRow" ng-model="runAnalysis.params.max_row">
                of {{ common.numberOfRows }} rows in total. (< 1000 is preferable.)
            </div>

            %{--Type of sorting to apply--}%
            <div class="heim-input-field sr-input-area">
                <h2>Used value:</h2>
                <fieldset class="heim-radiogroup">
                    <label>
                        <input type="radio" ng-model="runAnalysis.params.sorting" name="sortingSelect" value="patientnumbers"
                               checked> Patient Numbers
                    </label>
                    <label>
                        <input type="radio" ng-model="runAnalysis.params.sorting" name="sortingSelect" value="numericvalue">
                        Fetched Numeric Value
                    </label>
                </fieldset>
            </div>

            %{--Type of sorting to apply--}%
            <div class="heim-input-field  sr-input-area">
                <sorting-criteria criteria="runAnalysis.params.ranking"
                                  samples="common.totalSamples"
                                  subsets="common.subsets">
                </sorting-criteria>
            </div>

            <hr class="sr-divider">

            <run-button button-name="Create Plot"
                        store-results-in="runAnalysis.scriptResults"
                        script-to-run="run"
                        arguments-to-use="runAnalysis.params"
                        filename="phenotypeHeatmap.json"
                        running="runAnalysis.running">
            </run-button>
            <capture-plot-button filename="phenotypeHeatmap.svg" disabled="runAnalysis.download.disabled" target="phenotype-heatmap-plot">
            </capture-plot-button>
            <download-results-button disabled="runAnalysis.download.disabled"></download-results-button>
            <br/>
            <workflow-warnings warnings="runAnalysis.scriptResults.warnings"></workflow-warnings>
            <phenotype-heatmap-plot data="runAnalysis.scriptResults"
                          width="1200"
                          height="1200"
                          params="runAnalysis.params">
            </phenotype-heatmap-plot>

        </workflow-tab>

    </tab-container>
</div>
</script>
