import {h} from '@cycle/web';
import navbar from './navbar.js';

module.exports = {
  input, output
};

//// OUTPUT

let bgImage = require('../../statics/bg.jpg');

function output (model) {
  return h('div', {style:bgStyle()}, [
    navbar.output(false),
    !model.isUserLoading ? null : h('div.progress', {style:{margin:'0px'}}, h('div.indeterminate')),
    h('div.row.section', [
      h('div.col.s12.l7', explanation()),
      h('div.col.s12.m6.offset-m3.l4', loginForm('login', model)),
    ])
  ]);
}

function bgStyle () {
  return {
    'background-image': 'url(' + bgImage + ')',
    'background-position': 'center center',
    'background-attachment': 'fixed',
    'background-size': 'cover',
    'min-height': '1200px',
    'height': '100%'
  };
}

function explanation () {
  return h('div.container', h('div.section', [
    h('h2.white-text.hide-on-small-only', 
      {style:{'font-weight':'200'}}, 
      [h('b','Track'), ' your activities, improve your ', h('b', 'productivity')]
    ),
    h('h4.white-text.hide-on-med-and-up', 
      {style:{'font-weight':'200'}}, 
      [h('b','Track'), ' your activities, improve your ', h('b', 'productivity')]
    ),
  ]));
}

function card (title, content, action) {
  return h('div.card', [
    h('div.card-content', [
      h('span.card-title.black-text', title),
      content
    ]),
    h('div.card-action', action)
  ]);
}

function loginFormInput (formId, isDisabled) {
  return [
    h('div.row', h('div.input-field.col.s12', [
      h('input#username', {key: 1, type:'text', disabled:isDisabled}),
      h('label', 'Username')
    ])),
    h('div.row', h('div.input-field.col.s12', [
      h('input#password', {key: 1, type:'password', disabled:isDisabled}),
      h('label', 'Password')
    ]))
  ];
}

function loginFormAction (formId, isDisabled) {
  return [
    h('a#register.btn-flat.waves-effect.waves-grey.black-text.' + isDisabled, {disabled:isDisabled}, 'Register'),
    h('a#login.btn.waves-effect.waves-light.black-text.' + isDisabled, {disabled:isDisabled}, 'Login')
  ];
}

function loginForm (formId, model) {
  var isDisabled = model.isUserLoading ? 'disabled' : '';
  return card('Login', loginFormInput(formId, isDisabled), loginFormAction(formId, isDisabled));
}

//// INPUT

function input (DOM) {
  return {
    register$: parseLogin(DOM, '#register'),
    login$: parseLogin(DOM, '#login')
  };
}

function parseLogin (DOM, selector) {
  return DOM.get('a' + selector + ':not(.disabled)', 'click').map(() => {
      return {
        name: $('input#username').val(),
        pass: $('input#password').val()
      };
    })
    .filter(x => !!x.name && !!x.pass);
}