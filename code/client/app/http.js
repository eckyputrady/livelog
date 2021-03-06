import {makeHTTPDriver} from '@cycle/http';
import {Rx} from '@cycle/core';

module.exports = {
  driver, input, output
};

//// DRIVER

function driver () {
  return makeHTTPDriver();
}

//// INPUT

function input (HTTP$$) {
  let http$$ = HTTP$$.share();
  return {
    logAdded$: commonParse(6, http$$),
    // logRemoved$:
    logsLoaded$: commonParse(1, http$$),
    tagAdded$: commonParse(8, http$$),
    // tagRemoved$:
    tagsLoaded$: commonParse(2, http$$),
    taggingAdded$: commonParse(9, http$$),
    taggingRemoved$: commonParse(10, http$$),
    taggingsLoaded$: commonParse(11, http$$),
    userCreated$: commonParse(3, http$$),
    sessionCreated$: commonParse(4, http$$),
    sessionLoaded$: commonParse(5, http$$),
    sessionRemoved$: commonParse(7, http$$)
  };
}

function commonParse (type, http$$) {
  return http$$
    .filter(x$ => x$.request && x$.request.__type === type)
    .flatMap(x$ => {
      return x$ .map(x => { return { succ: x.body }; })
                .catch(x => { return Rx.Observable.just({ fail: x }); })
                .map(x => {
                  x.request = x$.request;
                  return x;
                });
    });
}

//// OUTPUT

function output (model, inputs) {
  return Rx.Observable.merge([
    loadLogs(model, inputs),
    createLog(model, inputs),
    loadTags(model, inputs),
    createTag(model, inputs),
    loadTaggings(model, inputs),
    createTagging(model, inputs),
    removeTagging(model, inputs),
    createUser(model, inputs),
    createSession(model, inputs),
    getSession(model, inputs),
    logout(model, inputs)
  ]);
}

function loadLogs ({curUser$, state$}, {logAdded$}) {
  let distinctState$ = state$.distinctUntilChanged();
  let distinctUser$ = curUser$.distinctUntilChanged();
  let trigger$ = Rx.Observable.combineLatest(distinctState$, distinctUser$, (state, user) => [state, user])
    .filter(([state, user]) => state === 'Logs' && !!user);
  let logAddedSucc$ = logAdded$.filter(x => !x.fail);

  return Rx.Observable.merge([trigger$, logAddedSucc$]).debounce(200).map(() => {
    return {
      __type: 1,
      method: 'GET',
      url: '/logs?sort=-createdAt&limit=50',
    };
  });
}

function createLog (model, {createLog$}) {
  return createLog$.map(data => {
    return {
      __type: 6,
      method: 'POST',
      url: '/logs',
      send: data
    };
  });
}

function loadTags ({curUser$, state$}, {tagAdded$}) {
  let distinctState$ = state$.distinctUntilChanged().filter(e => e === 'Tags');
  let distinctUser$ = curUser$.distinctUntilChanged().filter(e => !!e); 
  let tagAddedSucc$ = tagAdded$.filter(x => !x.fail);

  return Rx.Observable.merge([distinctState$, distinctUser$, tagAddedSucc$]).debounce(200).map(() => {
    return {
      __type: 2,
      method: 'GET',
      url: '/tags',
    };
  });
}

function createTag (model, {createTag$}) {
  return createTag$.map(data => {
    return {
      __type: 8,
      method: 'POST',
      url: '/tags',
      send: data
    };
  });
}

function loadTaggings (model, {logsLoaded$, taggingAdded$, taggingRemoved$}) {
  let taggingModified$ = Rx.Observable.merge([taggingRemoved$, taggingAdded$]).filter(x => !x.fail).map(x => x.request.send.logId);
  let t$ = logsLoaded$.filter(x => !x.fail).flatMap(({succ}) => Rx.Observable.from(succ)).map(log => log.id);
  return Rx.Observable.merge([taggingModified$, t$])
    .map(logId => {
      return {
        __type: 11,
        method: 'GET',
        url: '/logs/' + logId + '/tags',
        send: {logId: logId}
      };
    });
}

function createTagging (model, {createTagging$}) {
  return createTagging$.map(data => {
    return {
      __type: 9,
      method: 'POST',
      url: '/logs/' + data.logId + '/tags/' + data.tagId,
      send: data
    };
  });
}

function removeTagging (model, {removeTagging$}) {
  return removeTagging$.map(data => {
    return {
      __type: 10,
      method: 'DEL',
      url: '/logs/' + data.logId + '/tags/' + data.tagId,
      send: data
    };
  });
}

function createUser (model, {register$}) {
  return register$.map(data => {
    return {
      __type: 3,
      method: 'POST',
      url: '/users',
      send: data
    };
  });
}

function createSession (model, {userCreated$, login$}) {
  let afterRegister$ = userCreated$.filter(e => !!e.succ).map(e => e.request.send);
  return Rx.Observable.merge(login$, afterRegister$).map(data => {
    return {
      __type: 4,
      method: 'POST',
      url: '/sessions',
      send: data
    };
  });
}

function getSession ({curUser$}, {sessionCreated$}) {
  let afterLogin$ = sessionCreated$.filter(e => !e.fail);
  let nullUser$ = curUser$.filter(e => !e).take(1);
  return Rx.Observable.merge([afterLogin$, nullUser$]).map(() => {
    return {
      __type: 5,
      method: 'GET',
      url: '/sessions'
    };
  });
}

function logout (model, {logout$}) {
  return logout$.map(() => {
    return {
      __type: 7,
      method: 'DEL',
      url: '/sessions'
    };
  });
}