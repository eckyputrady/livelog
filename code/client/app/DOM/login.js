import {h} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {Rx} from '@cycle/core';
import navbar from './navbar.js'

module.exports = {
  input, output
};

//// OUTPUT

function output (model) {
  return h('div', [
    navbar.output(false),
    !model.isUserLoading ? null : h('div.progress', {style:{margin:'0px'}}, h('div.indeterminate')),
    h('div.container', h('div.section', loginForm('login', model)))
  ]);
}

function loginForm (formId, model) {
  var isDisabled = model.isUserLoading ? 'disabled' : '';
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
      h('div.col.s6', h('a#register.col.s12.btn-flat.waves-effect.waves-grey.' + isDisabled, {disabled:isDisabled}, 'Register')),
      h('div.col.s6', h('a#login.col.s12.btn.waves-effect.waves-light.' + isDisabled, {disabled:isDisabled}, 'Login'))
    ])
  ]));
}

//// INPUT

function input (DOM) {
  return {
    register$: parseLogin(DOM, '#register'),
    login$: parseLogin(DOM, '#login')
  };
}

function parseLogin (DOM, selector) {
  return DOM.get('form#login ' + selector + ':not(.disabled)', 'click').map(() => {
      return {
        name: $('form#login input#username').val(),
        pass: $('form#login input#password').val()
      };
    })
    .filter(x => !!x.name && !!x.pass);
}