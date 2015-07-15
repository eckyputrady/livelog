import {Rx} from '@cycle/core';
import _ from 'lodash';

module.exports = {
  update
};

function update (actions) {
  return model(actions);
}

// model
/**
  model = {
    state   :: State,
    sideFx  :: [SideEffect]
  }

  State :: {
    user :: Loadable (Maybe User),
    logs :: Loadable [Loadable Log]
    tags :: Loadable (Map TagKey (Loadable Tag))
    logTags :: Map LogKey (Loadable TagKey)
    state :: Logs | Tags
    loginForm :: { name :: String, pass :: String }
    logForm :: { name :: String }
  }

  User = { name :: String }
  Log = {
    id :: Int
    message :: String
    createdAt :: DateTime
  }
  Tag = {
    id :: Int
    name :: String
  }
  Loadable a = {
    isLoading :: Boolean
    isSucc :: Boolean
    sVal :: a
    eVal :: Obj
  }

  SideEffect= Login name pass 
            | Logout
            | Register name pass
            | CheckLogin
            | LoadLogs
            | CreateTag name
            | LoadTags
            | LoadLogTags
            | ShowInfo str
*/
function defaultModel () {
  return {
    state : defaultState(),
    sideFx: [{type:'checkLogin'}]
  };
}
function defaultState () {
  return {
    user: defaultLoadable(null, true),
    logs: defaultLoadable([]),
    tags: defaultLoadable({}),
    logTags: {},
    state: 'Logs',
    loginForm: {name: '', pass: ''}
  };
}
function defaultLoadable (val, isLoading) {
  return {
    isLoading: isLoading || false,
    isSucc: true,
    sVal: val,
    eVal: {}
  };
}

function model (actions) {
  let mergedActions = Rx.Observable.merge([
    Rx.Observable.just(defaultModel()).delay(200),

    actions.register$.map(setHandler(handleRegister)),
    actions.registerRes$.map(setHandler(handleRegisterRes)),

    actions.login$.map(setHandler(handleLogin)),
    actions.loginRes$.map(setHandler(handleLoginRes)),

    actions.checkLoginRes$.map(setHandler(handleCheckLoginRes)),

    actions.logout$.map(setHandler(handleLogout)),
    actions.logoutRes$.map(setHandler(handleLogoutRes)),

    actions.loadLogsRes$.map(setHandler(handleLoadLogsRes)),

    actions.createLog$.map(setHandler(handleCreateLog)),
    actions.createLogRes$.map(setHandler(handleCreateLogRes)),

    actions.createTag$.map(setHandler(handleCreateTag)),
    actions.createTagRes$.map(setHandler(handleCreateTagRes)),

    actions.loadTagsRes$.map(setHandler(handleLoadTagsRes)),

    actions.loadLogTagsRes$.map(setHandler(handleLoadLogTagsRes))
  ]);
  return mergedActions.scan(applyHandler);
}

//// Handler

function setHandler (f) {
  return function _setHandler (x) {
    x.__handler = f;
    return x;
  };
}
function applyHandler (model, action) {
  return action.__handler(model, action);
}

function handleRegister (model, action) {
  model.state.user.isLoading = true;
  model.sideFx = [{type: 'register',data: action}];
  return model;
}

function handleLogin (model, action) {
  model.state.user.isLoading = true;
  model.sideFx = [{type: 'login', data: action}];
  return model;
}

function handleRegisterRes (model, action) {
  model.state.user.isLoading = !action.fail;
  model.sideFx = [action.succ ? {type: 'checkLogin'} : {type: 'showInfo', data: action.fail}];
  return model;
}

function handleLoginRes (model, action) {
  model.state.user.isLoading = !action.fail;
  model.state.user.sVal = action.fail ? model.state.user.sVal : action.succ;
  model.sideFx = action.fail ? [{type: 'showInfo', data: action.fail}] : [{type: 'checkLogin'}];
  return model;
}

function handleCheckLoginRes (model, action) {
  model.state.user.isLoading = false;
  let oldUser = model.state.user.sVal;
  model.state.user.sVal = action.fail ? oldUser : action.succ;
  model.sideFx = !oldUser ? [{type: 'loadLogs'},{type: 'loadTags'}] : [];
  model.state.state = !oldUser ? 'Logs' : model.state.state;
  model.state.logs.isLoading = true;
  return model;
}

function handleLogout (model) {
  model.state.user.isLoading = true;
  model.state.user.sVal = null;
  model.sideFx = [{type: 'logout'}];
  return model;
}

function handleLogoutRes (model, action) {
  model.state.user.isLoading = false;
  model.state.user.sVal = null;
  model.sideFx = [{type: 'showInfo', data: action.fail || 'You\'re logged out!'}];
  return model;
}

function handleLoadLogsRes (model, action) {
  model.state.logs.isLoading = false;
  if (action.fail) {
    model.sideFx = [{type: 'showInfo', data: action.fail}];
  } else {
    model.state.logs.sVal = processLogs(action.succ);
    model.sideFx = _.map(action.succ, loadLogTagsSideFx);
  }
  return model;
}

function loadLogTagsSideFx (log) {
  return {
    type: 'loadLogTags',
    data: log.id
  };
}

function handleCreateLog (model, action) {
  model.state.logs.isLoading = true;
  model.sideFx = [{type: 'createLog', data: action}];
  return model;
}

function handleCreateLogRes (model, action) {
  model.sideFx = action.fail ? [{type: 'showInfo', data: action.fail}] : [{type: 'loadLogs'}];
  return model;
}

function handleCreateTag (model, action) {
  model.sideFx = [{type: 'createTag', data: action}];
  return model;
}

function handleCreateTagRes (model, action) {
  model.sideFx = action.fail ? [{type: 'showInfo', data: action.fail}] : [];
  return action.fail ? model : handleLoadTags(model);
}

function handleLoadTags (model) {
  model.state.tags.isLoading = true;
  model.sideFx = [{type: 'loadTags'}];
  return model;
}

function handleLoadTagsRes (model, action) {
  model.state.tags.isLoading = false;
  if (action.succ) {
    model.state.tags.sVal = processTags({}, action.succ);
    model.sideFx = [];
  } else {
    model.sideFx = [{type: 'showInfo', data: action.fail}];
  }
  return model;
}

function handleLoadLogTagsRes (model, action) {
  model.state.tags.sVal = processTags(model.state.tags.sVal, action.succ.tags);
  model.state.logTags = processLogTags(model.state.logTags, action.succ);
  model.sideFx = [];
  return model;
}

////

function processLogs (logs) {
  _.forEach(logs, (log, idx) => {
    log.duration = idx <= 0 ? undefined :
                              (new Date(logs[idx-1].createdAt) - new Date(log.createdAt));
  });
  return _.map(logs, defaultLoadable);
}

function processTags (initialTags, tags) {
  return _.merge(initialTags,  _.chain(tags)
                                .map((tag) => [tag.id, defaultLoadable(tag)])
                                .zipObject()
                                .value()
                );
}

function processLogTags (initialLogTags, logTags) {
  let lt = logTags;
  return _.merge(
    initialLogTags,
    _.zipObject([lt.logId, _.pluck(lt.tags, 'id')])
  );
}

//// Intent
/**
  register$ = {name :: String, pass :: String}
  login$ = {name :: String, pass :: String}
  loginRes$ = {succ = {name :: String}, fail :: String}
  registerRes$ = {succ = {name :: String, pass :: String}, fail :: String}
  checkLoginRes$ = {succ = {name :: String}, fail :: String}
  logout$ = {}
  logoutRes$ = {suce :: (), fail :: String},
  loadLogsRes$ = {succ :: [Log], fail :: String}
  createTag = {name :: String}
  createTagRes$ = {succ :: (), fail :: String} 
*/