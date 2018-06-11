<script type="text/ng-template" id="betadiversity">

<div ng-controller="BetaDiversityController">

    <tab-container>

        <workflow-tab tab-name="Fetch Data" disabled="fetch.disabled">
            <concept-box style="display: inline-block"
                         concept-group="fetch.conceptBoxes.datapoints"
                         type="LD-numerical"
                         min="25"
                         max="-1"
                         label="Numerical Variables for Distance Calculation"
                         tooltip="Select at least 25 numerical variables from the tree to be included in distance measures between patients.">
            </concept-box>
            <concept-box style="display: inline-block;"
                         concept-group="fetch.conceptBoxes.annotations"
                         type="LD-categorical"
                         min="1"
                         max="-1"
                         label="Categorical Variables for Annotation"
                         tooltip="Select an arbitrary amount of categorical variables from the tree to annotate the numerical datapoints.">
            </concept-box>
            <concept-box style="display: inline-block;"
                         concept-group="fetch.conceptBoxes.annotationsNumeric"
                         type="LD-numerical"
                         min="0"
                         max="-1"
                         label="Numerical Variables for Annotation"
                         tooltip="Select an arbitrary amount of numerical variables from the tree to annotate the numerical datapoints.">
            </concept-box>
            <br/>
            <br/>
            <fetch-button concept-map="fetch.conceptBoxes"
                          loaded="fetch.loaded"
                          running="fetch.running"
                          allowed-cohorts="[1,2]"> #2 cohorts: Error in dimnames(x) <- dn : length of 'dimnames' [1] not equal to array extent
            </fetch-button>
        </workflow-tab>

        <workflow-tab tab-name="Run Analysis" disabled="runAnalysis.disabled">
            <br/>
            <br/>
            <div class="heim-input-field sr-input-area">
                <h2>Distance Mode:</h2>
                <fieldset class="heim-radiogroup">
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="bray" checked> Bray-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="euclidean" > Euclidean-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="manhattan" > Manhattan-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="gower" > Gower-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="canberra" > Canberra-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="kulczynski" > Kulczynski-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="morisita" > Morisita-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="horn" > Horn-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="binomial" > Binomial-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="jaccard" > Jaccard-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="mountford" > Mountford-Index
                    </label>
                    <label>
                        <input type="radio"
                               ng-model="runAnalysis.params.inputmode"
                               value="raup" > Raup–Crick-Index
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
            <capture-plot-button filename="betadiversity.svg" target="betadiversity"></capture-plot-button>
            <br/>
            <br/>
            <betadiversity data="runAnalysis.scriptResults" width="1000" height="500"></betadiversity>
        </workflow-tab>

    </tab-container>

</div>

</script>
