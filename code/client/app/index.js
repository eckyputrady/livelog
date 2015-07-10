"use strict";

require('jquery');
require('npm/materialize-css/bin/materialize.css');
require('npm/materialize-css/bin/materialize.js');
import Cycle from '@cycle/core';
import {h, makeDOMDriver} from '@cycle/web';
import _ from 'lodash';

// main
document.querySelector('body').appendChild(document.createElement('div'));
Cycle.run(main, {
  DOM: makeDOMDriver('body > div')
});

function main (responses) {
  return {
    DOM: responses.DOM.get('input', 'change')
          .map(ev => ev.target.checked)
          .startWith(defaultModel())
          .map(view)
  };
}

// model
/**
  model = {
    user :: Loadable (Maybe User),
    logs :: Loadable [Loadable Log]
    tags :: Loadable [Loadable Tag]
    state :: Logs | Tags
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
*/
function defaultModel () {
  return {
    user: defaultLoadable({}),
    logs: defaultLoadable([]),
    tags: defaultLoadable([]),
    state: 'Logs'
  };
}
function defaultLoadable (val) {
  return {
    isLoading: false,
    isSucc: true,
    sVal: val,
    eVal: {}
  };
}

// views

function view (model) {
  return !model.user.sVal ? loginView(model) : loggedInView(model);

  // return h('div', [
  //   navbar(),
  //   content(),
  //   fab(),
  //   createTagModal()
  // ]);
}

function loginView (model) {
  return h('div', [
    navbar(false),
    !model.user.isLoading ? null : h('div.progress', h('div.indeterminate')),
    h('div.container', h('div.section', loginForm('login', model)))
  ]);
}

function loggedInView (model) {
  return h('div', [
    navbar(true),
    h('div.container.section', currentLogView(model)),
    h('div.section', [
      pastLogsView(model)
    ])
  ]);
}

function pastLogsView (model) {
  let item = {message: 'Sketching UX', createdAt: new Date(), tags: []};
  return h('ul.collection.z-depth-1', [
    logItemView(item),
    logItemView(item)
  ]);
}

function logItemView (item) {
  return h('li.collection-item.avatar', [
    h('i.large.material-icons.circle.red', 'done'),
    h('span.title', item.message),
    h('p', ['05/04/2015 01:32:12', h('br'), labels()]),
    h('span.secondary-content', '01:02:05')
  ]);
}

function currentLogView (model) {
  return h('div.row', [
    h('h1.col.s12.center', '01:32:59'),
    h('h4.col.s12.center', 'Sketching UX'),
    h('div.col.s12.center', labels()),
    h('p.col.s12.center', 'starting 2015-10-04')
  ]);
}

function loginForm (formId, model) {
  var isDisabled = model.user.isLoading ? 'disabled' : '';
  return h('div.row', h(`form.col.s12#${formId}`, [
    h('h4', 'Login'),
    h('div.row', h('div.input-field.col.s12', [
      h('input#username', {type:'text', disabled:isDisabled}),
      h('label', 'Username')
    ])),
    h('div.row', h('div.input-field.col.s12', [
      h('input#password', {type:'password', disabled:isDisabled}),
      h('label', 'Password')
    ])),
    h('div.row', [
      h('div.col.s6', h('a.col.s12.btn-flat.waves-effect.waves-teal.' + isDisabled, {disabled:isDisabled}, 'Register')),
      h('div.col.s6', h('a.col.s12.btn.waves-effect.waves-light.' + isDisabled, {disabled:isDisabled}, 'Login'))
    ])
  ]));
}

function navbar (withSideNav) {
  return h('nav', [
    h('div.container', [
      withSideNav ? sideNav() : null,
      h('div.nav-wrapper', [
        h('a.brand-logo', 'LOGO')
      ])
    ])
  ]);
}

function sideNav () {
  setTimeout(() => $('.button-collapse').sideNav(), 200);
  return [
    h('ul#sideNav.side-nav', [
      h('li', h('a', 'Logs')),
      h('li', h('a', 'Tags')),
      h('li.divider'),
      h('li', h('a', 'Logout'))
    ]),
    h('a.button-collapse', {attributes:{'data-activates':'sideNav'}}, h('i.mdi-navigation-menu'))
  ];
}

function label (name) {
  return h('span.teal.lighten-3', {style:{padding:'3px', margin:'2px'}}, name)
}

function labels () {
  return [
    label('haskell'),
    label('rest'),
    labelInput()
  ];
}

function labelInput () {
  setTimeout(initDropdown, 200);
  var randId = 'dropdown-' + new Date().getTime();
  return h('span', [
    h('a.dropdown-button.teal.lighten-4', {href:'#', attributes:{'data-activates':randId}, style:{padding:'3px', margin:'2px'}}, '+'),
    h('ul#' + randId + '.dropdown-content', [
      h('li', h('a', 'one')),
      h('li', h('a', 'one')),
      h('li', h('a', 'one'))
    ])
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

/*
function fab () {
  return h('div.fixed-action-btn', {style:{bottom:'45px',right:'24px'}}, [
    h('a.btn-floating.btn-large.waves-effect.waves-light.red',[
      h('span.material-icons', '+')
    ])
  ]);
}

function content () {
  return h('div.container', [
    h('div.section', currentLog()),
    h('div.section', tabs([
      {title: 'Logs', content: pastActivities()},
      {title: 'Tags', content: tags()},
      {title: 'Stats', content: stats()}
    ]))
  ]);
}

function tags () {
  return h('div.row', [
    h('div.col.s12', [
      h('table', [
        h('thead', [
          h('tr', [
            h('th', 'Tags'),
          ])
        ]),
        h('tbody', [
          h('tr', [
            h('td', 'haskell'),
          ]),
          h('tr', [
            h('td', h('input', {type:'text'})),
          ])
        ])
      ])
    ])
  ])
}

function stats () {
  return [];
}

function tabs (objs) {
  setTimeout(() => $('ul.tabs').tabs(), 200);
  var id = 'tab-' + new Date().getTime();
  return h('div.row', [
    h('div.col.s12', [
      h('ul.tabs.z-depth-1', [
        _.map(objs, (x, idx) => h('li.tab.col.s4', h('a', {href:'#'+id+idx}, x.title)))
      ]),
      _.map(objs, (x, idx) => h('div.#' + id+idx + '.col.s12', x.content))
    ])
  ]);
}

function pastActivities () {
  return h('div.row', [
    h('div.col.s12', [
      h('table', [
        h('thead', [
          h('tr', [
            h('th', 'Message'),
            h('th', 'Tags'),
            h('th', 'Duration'),
          ])
        ]),
        h('tbody', [
          h('tr', [
            h('td', 'Writing front end code'),
            h('td', labels()),
            h('td', '01:00:20')
          ])
        ])
      ])
    ])
  ])
}

function currentLog () {
  return h('div.row', [
    h('div.col.s8', h('h2', 'I do this!')),
    h('div.col.s4.valign-wrapper', h('h2.valign', '12:33:10')),
    h('div.col.s12'),
    labels()
  ]);
}


function createTagModal () {
  setTimeout(initModal, 200);
  return h('div#create-tag-modal.modal', [
    h('div.modal-content', [
      h('h4', 'Create a New Tag')
    ]),
    h('div.modal-footer', [
      h('a.modal-action.waves-effect.btn-flat', 'Create')
    ])
  ]);
}

function initModal () {
  console.log('called!');
  $('.modal-trigger').leanModal();
}

*/