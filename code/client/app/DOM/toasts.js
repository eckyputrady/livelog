import {Rx} from '@cycle/core';

module.exports = {
  output
};

////

function output (model, inputs) {
  return Rx.Observable.merge([
      errorMsg(inputs),
      userCreatedMsg(inputs),
      sessionCreatedMsg(inputs),
      sessionRemovedMsg(inputs),
      tagCreatedMsg(inputs)
    ])
    .subscribe(e => Materialize.toast(e, 3000));
}

function errorMsg (inputs) {
  return Rx.Observable.merge([
      inputs.userCreated$,
      inputs.sessionCreated$,
      inputs.logsLoaded$,
      inputs.logAdded$
    ])
    .filter(e => e.fail)
    .map(e => e.fail.message);
}

function tagCreatedMsg (inputs) {
  return filterSucc(inputs.tagAdded$).map(x => `'${x.request.send.name}' is created!`);
}

function userCreatedMsg (inputs) {
  return filterSucc(inputs.userCreated$).map((x) => `'${x.request.send.name}' is registered successfully!`);
}

function sessionCreatedMsg (inputs) {
  return filterSucc(inputs.sessionLoaded$).map((x) => `Hi, ${x.succ.name}!`);
}

function sessionRemovedMsg (inputs) {
  return filterSucc(inputs.sessionRemoved$).map(() => 'Good bye!');
}

function filterSucc (obs) {
  return obs.filter(x => !x.fail);
}