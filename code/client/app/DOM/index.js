require('jquery');
require('hammerjs');
require('npm/materialize-css/bin/materialize.css');
require('npm/materialize-css/bin/materialize.js');

import {h, makeDOMDriver} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {Rx} from '@cycle/core';
import {trace} from '../util.js';
import login from './login.js';
import logs from './logs.js';
import modals from './modals.js';
import navbar from './navbar.js';

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
    modals.input(DOM),
    navbar.input(DOM)
  ];

  return _.reduce(inputs, _.merge, {});
}

//// OUTPUT

function output (model, inputs) {
  showToast$(model, inputs);
  return render$(model, inputs);
}

function showToast$ (model, inputs) {
  let errors$ = Rx.Observable.merge([
      inputs.userCreated$,
      inputs.sessionCreated$
    ])
    .filter(e => e.fail)
    .map(e => e.fail.message);

  let userCreatedMsg$ = inputs.userCreated$.filter(x => x.succ).map((x) => `'${x.request.send.name}' is registered successfully!`);
  let sessionCreatedMsg$ = inputs.sessionCreated$.filter(x => x.succ).map((x) => `Hi, ${x.request.send.name}!`);

  return Rx.Observable.merge([
      errors$,
      userCreatedMsg$,
      sessionCreatedMsg$
    ])  
    .subscribe((e) => Materialize.toast(e, 3000));
}

function render$ (model, inputs) {
  let curTime$ = timeEvery1Sec();
  let model$ = snapModel(curTime$, model);
  let sampledModel$ = model$.sample(1000 / 30); // draw every 30FPS & IF there is any change
  return sampledModel$.map(render);
}

function timeEvery1Sec () {
  return Rx.Observable.interval(1000).startWith(0).map(_ => new Date());
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
          h('div', 'asdasds');
}

// TAGS

function tagsView (model) {
  return [
    h('div.row', h('div.col.s12', h('h3', 'Tags'))),
    h('div', [
      tagsListing(model),
      h('div.center', circleLoader(model.tags.isLoading))
    ])
  ];
}

function tagsListing (model) {
  return h('ul.collection', [
    _.map(model.tags.sVal, tagItem)
  ]);
}

function tagItem (tag) {
  return h('li.collection-item', [
    h('div', [
      tag.sVal.name,
      h('a.secondary-content', h('i.material-icons', 'delete'))
    ])
  ]);
}