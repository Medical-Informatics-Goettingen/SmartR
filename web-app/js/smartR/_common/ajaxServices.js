//# sourceURL=ajaxServices.js

smartR.ajaxServices = function(basePath, workflow) {
    var result = {};

    var NOOP_ABORT = function() {};
    var TIMEOUT = 10000 /* 10 s */;
    var CHECK_DELAY = 1000;

    /* we can only support on request at a time */

    var state = {
        currentRequestAbort: NOOP_ABORT,
        sessionId: null
    };

    /* returns a promise with the session id and
     * saves the session id for future calls */
    result.startSession = function ajaxServices_startSession() {
        return jQuery.ajax({
            url: basePath + '/RSession/create',
            type: 'POST',
            timeout: TIMEOUT,
            contentType: 'application/json',
            data: JSON.stringify( {
                workflow: workflow
            })
        }).then(function(response) {
            return response.sessionId;
        }).done(function(sessionId) {
            state.sessionId = sessionId;
        });
    };

    result.destroySession = function ajaxServices_destroySession(sessionId) {
        sessionId = sessionId || state.sessionId;

        if (!sessionId) {
            throw new Error('No session to destroy');
        }

        return jQuery.ajax({
            url: basePath + '/RSession/create',
            type: 'POST',
            timeout: TIMEOUT,
            contentType: 'application/json',
            data: JSON.stringify( {
                sessionId: sessionId
            })
        }).done(function() {
            state.sessionId = null;
        });
    };

    /*
     * taskData = {
     *     arguments: { ... },
     *     taskType: 'fetchData' or name of R script minus .R,
     *     phase: 'fetch' | 'preprocess' | 'run',
     * }
     *
     * If it succeeds, the result will be the data returned by the server.
     * If it fails, it will return an object with at least these fields:
     * {
     *   status: 0 | <http error code>,
     *   statusText: <a description of the error>,
     * }
     */
    result.startScriptExecution = function ajaxServices_startScriptExecution(taskDataOrig) {

        var taskData = jQuery.extend({}, taskDataOrig); // clone the thing
        state.currentRequestAbort();

        var runRequest = jQuery.ajax({
            url: basePath + '/ScriptExecution/run',
            type: 'POST',
            timeout: TIMEOUT,
            contentType: 'application/json',
            data: JSON.stringify({
                sessionId: state.sessionId,
                arguments: taskData.arguments,
                taskType: taskData.taskType,
                workflow: workflow
            })
        });

        _setCancellationForAjaxCall(runRequest);

        /* schedule checks and replace promise */
        runRequest
            .then(function(d) {
                taskData.executionId = d.executionId;
                return _checkStatus(taskData.executionId, CHECK_DELAY);
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                return errorThrown;
            });

        return runRequest;
    };

    function _setCancellationForAjaxCall(ajax) {
        /* request in-flight; aborting is cancelling this request */
        state.currentRequestAbort = function() { ajax.abort(); };
        /* once the request finishes, there's nothing to abort.
         * this needs to be the 1st callback, so that later callbacks can
         * override this */
        ajax.always(function() {
            state.currentRequestAbort = NOOP_ABORT;
        });
    }

    /* set a function to be executed after a period of time and
     * simultaneously define the request cancellation function
     * as the cancellation of this timeout.
     * Return the result as a promise. */
    function _setStatusRequestTimeout(funcToCall, delay /*, ... */) {
        var defer = jQuery.Deferred();
        var promise = defer.promise();

        function _setStatusRequestTimeout_wrappedFunc() {
            var restOfArguments = Array.prototype.slice.call(arguments)
                .splice(2); // arguments after delay

            // we cannot abort by calling clearTimeout() anymore
            // funcToCall will probably set its own abort method
            state.currentRequestAbort = NOOP_ABORT;

            // funcToCall should itself return a promise or a final result
            // but let's cover the case where it throws
            var result;
            try {
                result = funcToCall.apply(undefined, restOfArguments);
                defer.resolve(result);
            } catch (e) {
                defer.fail(e);
            }
        }

        var timeout = window.setTimeout(
            _setStatusRequestTimeout_wrappedFunc, delay);

        state.currentRequestAbort = function() {
            clearTimeout(timeout);
            state.currentRequestAbort = NOOP_ABORT;
        };

        promise.cancel = function timeoutRequest_cancel() {
            // calling this method will by itself resolve the promise
            // in case there's an ajax call pending.
            state.currentRequestAbort();
            // if there is a timeout pending, it won't though
            if (defer.state() == 'pending') {
                defer.fail(new Error('Request was aborted'));
        }
        };

        return promise;
    }

    /* aux function of _startScriptExecution. Needs to follow its contract
     * with respect to the fail and success result of the promise */
    function _checkStatus(executionId, delay) {
        var ajax = jQuery.ajax({
            type: 'GET',
            url : basePath + '/ScriptExecution/status',
            data: {
                sessionId  : state.sessionId,
                executionId: executionId
            }
        });

        state.currentRequestAbort = function() { ajax.abort(); };
        ajax.always(function() { state.currentRequestAbort = NOOP_ABORT; });

        return ajax.then(function (d) {
            if (d.state === 'FINISHED') {
                return d;
            } else if (d.state === 'FAILED') {
                return jQuery.Deferred().reject({
                    status: 0,
                    statusText: d.result.exception
                }).promise();
            } else {
                // else still pending
                return _setStatusRequestTimeout(
                    _checkStatus, delay, executionId, delay);
            }
        }).fail(function(jqXHR, textStatus, errorThrown) {
            return errorThrown;
        });
    }


    result.downloadJsonFile = function(executionId, filename) {
        return jQuery.ajax({
            url: _urlForFile(executionId, filename),
            dataType: 'json'
        });
    };


    function _urlForFile(executionId, filename) {
        return basePath +
            '/ScriptExecution/downloadFile?sessionId=' +
            state.sessionId +
            '&executionId=' +
            executionId +
            '&filename=' +
            filename;
    }

    return result;
};
