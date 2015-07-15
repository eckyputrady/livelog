import {Rx} from '@cycle/core';

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
    tags :: Loadable [Loadable Tag]
    state :: Logs | Tags
    loginForm :: { name :: String, pass :: String }
    logForm :: { name :: String }
  }

  User = { name :: String }
  Log = {
    id :: Int
    message :: String
    createdAt :: DateTime
    tags :: Loadable [Loadable Tag]
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
    tags: defaultLoadable([]),
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
function dummyLogs () {
  return {
    id: 1,
    message: 'Dummy log',
    createdAt: new Date(),
    tags: defaultLoadable([defaultLoadable(dummyTags()), defaultLoadable(dummyTags()), defaultLoadable(dummyTags())])
  };
}
function dummyTags () {
  return {
    id: 1,
    name: 'Dummy tag'
  };
}

function model (actions) {
  let mergedActions = Rx.Observable.merge(
    Rx.Observable.just(defaultModel()).delay(200),

    actions.register$.map(setHandler(handleRegister)),
    actions.registerRes$.map(setHandler(handleRegisterRes)),

    actions.login$.map(setHandler(handleLogin)),
    actions.loginRes$.map(setHandler(handleLoginRes)),

    actions.checkLoginRes$.map(setHandler(handleCheckLoginRes)),

    actions.logout$.map(setHandler(handleLogout)),
    actions.logoutRes$.map(setHandler(handleLogoutRes)),

    actions.loadLogsRes$.map(setHandler(handleLoadLogsRes))
  );
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
  model.sideFx = !oldUser ? [{type: 'loadLogs'}] : [];
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
    model.state.logs.sVal = _.map(action.succ, defaultLoadable);
    model.sideFx = [];
  }
  return model;
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
*/