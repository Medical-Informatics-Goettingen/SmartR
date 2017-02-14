<script type="text/ng-template" id="alphadiv">

<div ng-controller="AlphadivController">

    <tab-container>

        <workflow-tab tab-name="Fetch Data" disabled="fetch.disabled">
            <concept-box style="display: inline-block"
                         concept-group="fetch.conceptBoxes.datapoints"
                         type="LD-numerical"
                         min="2"
                         max="-1"
                         label="Numerical Variables"
                         tooltip="Select two numerical variables from the tree to compare them.">
            </concept-box>
            <concept-box style="display: inline-block;"
                         concept-group="fetch.conceptBoxes.annotations"
                         type="LD-categorical"
                         min="0"
                         max="-1"
                         label="(optional) Categorical Variables"
                         tooltip="Select an arbitrary amount of categorical variables from the tree to annotate the numerical datapoints.">
            </concept-box>
            <br/>
            <br/>
            <fetch-button concept-map="fetch.conceptBoxes"
                          loaded="fetch.loaded"
                          running="fetch.running"
                          allowed-cohorts="[1]">
            </fetch-button>
        </workflow-tab>

        <workflow-tab tab-name="Run Analysis" disabled="runAnalysis.disabled">
            <br/>
            <br/>
            <div class="heim-input-field sr-input-area">
                <h2>Alpha Diversity Mode:</h2>
                <fieldset class="heim-radiogroup">
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="shannon" checked> Shannon-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="simpson" > Simpson-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="invsimpson" > Inverse-Simpson-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="chao1" > Chao1-Estimator
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="obs" > Observed Species
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="ACE" > ACE-Estimator
                    </label>
                </fieldset>
            </div>
            <hr class="sr-divider">
            <run-button button-name="Create Plot"
                        store-results-in="runAnalysis.scriptResults"
                        script-to-run="run"
                        arguments-to-use="runAnalysis.params"
                        running="runAnalysis.running">
            </run-button>
            %{--<capture-plot-button filename="boxplot.svg" target="boxplot"></capture-plot-button>--}%
            <br/>
            <br/>
            <alphadiv data="runAnalysis.scriptResults" width="1000" height="500"></alphadiv>
        </workflow-tab>

    </tab-container>

</div>

</script>
