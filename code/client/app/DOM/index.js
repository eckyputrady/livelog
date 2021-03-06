require('jquery');
require('hammerjs');
require('npm/materialize-css/bin/materialize.css');
require('npm/materialize-css/bin/materialize.js');

import {makeDOMDriver} from '@cycle/web';
import _ from 'lodash';
import {Rx} from '@cycle/core';
import login from './login.js';
import logs from './logs.js';
import tags from './tags.js';
import modals from './modals.js';
import navbar from './navbar.js';
import toast from './toasts.js';

module.exports = {
  input, output, driver
};

//// DRIVER

function driver (selector) {
  return makeDOMDriver(selector);
}

//// INPUT

function input (DOM) {
  let inputs = [
    login.input(DOM),
    logs.input(DOM),
    tags.input(DOM),
    modals.input(DOM),
    navbar.input(DOM)
  ];

  return _.reduce(inputs, _.merge, {});
}

//// OUTPUT

function output (model, inputs) {
  toast.output(model, inputs);
  return render$(model);
}

function render$ (model) {
  let curTime$ = timeEvery1Sec();
  let model$ = snapModel(curTime$, model);
  let sampledModel$ = model$.sample(1000 / 30); // draw every 30FPS & IF there is any change
  return sampledModel$.map(render);
}

function timeEvery1Sec () {
  return Rx.Observable.interval(1000).startWith(0).map(() => new Date());
}

function snapModel (time$, model) {
  return Rx.Observable.combineLatest(
    time$, model.state$, model.logGroups$, model.logs$, model.tags$, model.taggings$, model.curUser$,
    model.isUserLoading$, model.curLogId$,
    (time, state, logGroups, logs, tags, taggings, curUser, isUserLoading, curLogId) => {
      return {
        time: time,
        state: state,
        logGroups: logGroups,
        logs: logs,
        tags: tags,
        taggings: taggings,
        user: curUser,
        isUserLoading: isUserLoading,
        curLogId: curLogId
      };
    });
}

function render (model) {
  return  !model.user ? login.output(model) : 
          model.state === 'Logs' ? logs.output(model) :
          tags.output(model);
}