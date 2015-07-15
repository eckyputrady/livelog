require('jquery');
require('hammerjs');
require('npm/materialize-css/bin/materialize.css');
require('npm/materialize-css/bin/materialize.js');

import {h, makeDOMDriver} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {Rx} from '@cycle/core';

module.exports = {
  input, output, driver
};

//// DRIVER

function driver (selector) {
  return makeDOMDriver(selector);
}

//// INPUT

function input (DOM) {
  return {
    register$: parseLogin(DOM, '#register'),
    login$: parseLogin(DOM, '#login'),
    logout$: parseLogout(DOM),
    createLog$: parseCreateLog(DOM),
    createTag$: parseCreateTag(DOM),
    changeState$: parseChangeState(DOM)
  };
}

function parseLogin (DOM, selector) {
  return DOM.get('form#login ' + selector + ':not(.disabled)', 'click').map(() => {
    return {
      name: $('form#login input#username').val(),
      pass: $('form#login input#password').val()
    };
  });
}

function parseCreateLog (DOM) {
  return DOM.get('#create-log:not(.disabled)', 'click').map(() => {
    return {
      message: $('#create-log-name').val()
    };
  });
}

function parseCreateTag (DOM) {
  return DOM.get('#create-tag:not(.disabled)', 'click').map(() => {
    return {
      name: $('#create-tag-name').val()
    };
  });
}

function parseLogout (DOM) {
  return DOM.get('a#logout', 'click');
}

function parseChangeState (DOM) {
  return DOM.get('a#change-state', 'click')
    .map(e => {
      return {
        nextstate: $(e.target).data('nextstate')
      };
    });
}


////////////////////////////////////////////////////////////////////////////////////////////


//// OUTPUT

function output (model$) {
  let m1 = model$.map(applyFx).map(model => model.state);
  let m2 = Rx.Observable.interval(1000);
  return Rx.Observable.combineLatest(m1, m2, (model) => {
    return !model.user.sVal ? loginView(model) : loggedInView(model);
  });
}

function applyFx (model) {
  _.forEach(model.sideFx, x => {
    switch(x.type) {
      case 'showInfo': 
        Materialize.toast(x.data, 4000);
        break;
      case 'logout':
        $('.button-collapse').sideNav('hide');
        break;
      default:
        break;
    }
  });

  return model;
}

// LOGIN

function loginView (model) {
  hasModalsInit = false; // due to materialize being stateful 

  return h('div', [
    navbar(false),
    !model.user.isLoading ? null : h('div.progress', {style:{margin:'0px'}}, h('div.indeterminate')),
    h('div.container', h('div.section', loginForm('login', model)))
  ]);
}

function loginForm (formId, model) {
  var isDisabled = model.user.isLoading ? 'disabled' : '';
  return h('div.row', h(`form.col.s12#${formId}`, [
    h('h4', 'Login'),
    h('div.row', h('div.input-field.col.s12', [
      h('input#username', {key: 1, type:'text', disabled:isDisabled}),
      h('label', 'Username')
    ])),
    h('div.row', h('div.input-field.col.s12', [
      h('input#password', {key: 1, type:'password', disabled:isDisabled}),
      h('label', 'Password')
    ])),
    h('div.row', [
      h('div.col.s6', h('a#register.col.s12.btn-flat.waves-effect.waves-teal.' + isDisabled, {disabled:isDisabled}, 'Register')),
      h('div.col.s6', h('a#login.col.s12.btn.waves-effect.waves-light.' + isDisabled, {disabled:isDisabled}, 'Login'))
    ])
  ]));
}

// LOGGED IN

function loggedInView (model) {
  return h('div', [
    navbar(true),
    model.state === 'Logs' ? logsView(model) : tagsView(model),
    fab(),
    modals()
  ]);
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

// LOGS

function logsView (model) {
  return [
    h('div.container.section', currentLogView(model)),
    h('div.section', [
      pastLogsView(model),
      h('div.center', circleLoader(model.logs.isLoading))
    ])
  ];
}

function circleLoader (isActive, size) {
  let active = isActive ? '.active' : '';
  let sizeClass = '.' + size;
  return h('div.preloader-wrapper' + active + sizeClass, h('div.spinner-layer.spinner-blue-only', [
    h('div.circle-clipper.left', h('div.circle')),
    h('div.gap-patch', h('div.circle')),
    h('div.circle-clipper.right', h('div.circle'))
  ]));
}

function pastLogsView (model) {
  let logs = model.logs.sVal;
  return logs.length <= 0 ? [] : h('ul.collection.z-depth-1', _.map(logs, (x) => logItemView(x, model.tags)));
}

function logItemView (logL, tagLsL) {
  let m = logL;
  let dur = !m ? 0 : (m.sVal.duration || (new Date() - new Date(m.sVal.createdAt)));
  let mm = {
    createdAt: moment(m.sVal.createdAt).format('YYYY/MM/DD HH:mm:ss'),
    message: m.sVal.message,
    duration: moment.utc(dur).format('H:mm:ss'),
  };
  return h('li.collection-item.avatar', {style: {height:'initial'}}, [
    h('i.large.material-icons.circle.red', 'done'),
    h('span.title', mm.message),
    h('p', [mm.createdAt, h('br'), /*labels(logL.sVal, tagLsL.sVal)*/]),
    h('span.secondary-content', mm.duration)
  ]);
}

function currentLogView (model) {
  let m = model.logs.sVal[0];
  let dur = !m ? 0 : (m.sVal.duration || (new Date() - new Date(m.sVal.createdAt)));
  let mm = m ? {
      createdAt: moment(m.sVal.createdAt).format('YYYY/MM/DD hh:mm:ss'),
      message: m.sVal.message,
      duration: moment.utc(dur).format('H:mm:ss'),
      labels: m.sVal.labels
    } : {
      createdAt: '-',
      message: 'You have not logged in anything',
      duration: `--:--:--`,
      // labels: defaultLoadable([])
    };
  return h('div.row', [
    h('h1.col.s12.center', mm.duration),
    h('h4.col.s12.center', mm.message),
    // h('div.col.s12.center', m ? labels(m.sVal, model.tags.sVal) : []),
    h('p.col.s12.center', ['since ', h('b', mm.createdAt)])
  ]);
}

function navbar (withSideNav) {
  return h('nav', [
    h('div.container', [
      sideNav(withSideNav),
      h('div.nav-wrapper', [
        h('a.brand-logo', 'LiveLog')
      ])
    ])
  ]);
}

let hasSidenavInit = false;
function initSidenav () {
  // the sidenav is a stateful operation, you can't add more than once.
  if (!hasSidenavInit) { $('.button-collapse').sideNav(); }
  hasSidenavInit = true;
}

function sideNav (visible) {
  setTimeout(initSidenav, 200);
  let extraClass = visible ? '' : '.hide';
  return [
    h('ul#sideNav.side-nav', [
      h('li', h('a#change-state', {attributes:{'data-nextState':'Logs'}}, 'Logs')),
      h('li', h('a#change-state', {attributes:{'data-nextState':'Tags'}}, 'Tags')),
      h('li.divider'),
      h('li', h('a#logout', 'Logout'))
    ]),
    h('a.button-collapse' + extraClass, {attributes:{'data-activates':'sideNav'}}, h('i.mdi-navigation-menu'))
  ];
}

function label (tagL) {
  let {name} = tagL.sVal || {name:''};
  return h('span.red.accent-1.z-depth-1', {style:{display:'inline-block', padding:'3px', margin:'2px'}}, [
    name, ' ', h('a', '\u2717')
  ]);
}

function labels (log, tagLs) {
  return [_.map(log.tags.sVal, label), labelInput(log, tagLs)];
}

function labelInput (log, tagLs) {
  // setTimeout(initDropdown, 200);
  var randId = 'dropdown-' + new Date().getTime();
  return h('span', [
    h('a.dropdown-button.teal.lighten-4.z-depth-1', {href:'#', attributes:{'data-activates':randId}, style:{padding:'5px 10px', margin:'2px'}}, '+'),
    h('ul#' + randId + '.dropdown-content', _.map(tagLs, (tag) => 
      h('li', h('a', tag.sVal.name))
    ))
  ]);
}

function initDropdown () {
  $('.dropdown-button').dropdown({
      inDuration: 300,
      outDuration: 225,
      constrain_width: false,
      gutter: 0, // Spacing from edge
      belowOrigin: false // Displays dropdown below the button
    }
  );
}

function modal (id, content, footer) {
  return h('div#' + id + '.modal', [
    h('div.modal-content', content),
    h('div.modal-footer', footer)
  ]);
}

function logDialogModal () {
  return modal('log-dialog', [
    h('h4', 'Log An Activity'),
    h('form', h('div.row', [
      h('div.input-field.s12', [
        h('input#create-log-name', {type:'text'}),
        h('label', 'Activity name')
      ])
    ]))
  ], [
    h('a#create-log.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Create'),
    h('a.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Cancel'),
  ]);
}

function tagDialogModal () {
  return modal('tag-dialog', [
    h('h4', 'Create Tag'),
    h('form', [
      h('div.input-field', [
        h('input#create-tag-name', {type:'text'}),
        h('label', 'Tag name')
      ])
    ])
  ], [
    h('a#create-tag.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Create'),
    h('a.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Cancel'),
  ]);
}

function fab () {
  return h('div.fixed-action-btn', {style:{bottom:'45px',right:'24px'}}, [
    h('a.btn-floating.btn-large.red', h('i.large.material-icons', 'add')),
    h('ul', [
      h('li', h('a.modal-trigger.btn-floating.red', {href:'#log-dialog'}, h('i.small.material-icons', 'done'))),
      h('li', h('a.modal-trigger.btn-floating.red', {href:'#tag-dialog'}, h('i.small.material-icons', 'label')))
    ])
  ]);
}

function modals () {
  setTimeout(initModals, 200);
  return [
    logDialogModal(),
    tagDialogModal()
  ];
}

let hasModalsInit = false;
function initModals () {
  if (hasModalsInit) return;
  hasModalsInit = true;
  $('.modal-trigger').leanModal()
}