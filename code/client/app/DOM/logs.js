import {h} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {loggedInContainer, circleLoader} from './common.js';

module.exports = {
  input, output
};

//// OUTPUT

function output (model) {
  return loggedInContainer(logsView(model));
}

function logsView (model) {
  return [
    h('div.container.section', currentLogView(model)),
    h('div.container.section', [
      logGroupsView(model),
      h('div.center', circleLoader(model.logs.isLoading))
    ])
  ];
}

function logGroupsView (model) {
  return _.map(model.logGroups, _.curry(logGroupView)(model));
}

function logGroupView (model, logs, date) {
  return h('div', [
    h('h5', date),
    pastLogsView(model, logs)
  ]);
}

function pastLogsView (model, logs) {
  return  logs.length <= 0 ? [] : 
          h('ul.collapsible.z-depth-1', 
            {
              'hook-init': new initCollapsibleHook(),
              attributes:{'data-collabsible':'accordion'}
            },
            _.map(logs, _.curry(logItemView)(model)));
}

function initCollapsibleHook () {}
initCollapsibleHook.prototype.hook = (node, name, prevValue) => {
  if (!prevValue) {
    setTimeout(() => $(node).collapsible(), 200);
  }
};

function logItemView (model, {logId}) {
  let log = model.logs[logId];
  return h('li', [
    h('div.collapsible-header', [
      h('i.material-icons.large' + colorBasedOnTime(log.createdAt), 'album'),
      h('span', log.message),
      h('span.right-align', {style:{float:'right'}}, moment(log.createdAt).format('h:mm A'))
    ]),
    h('div.collapsible-body.grey.lighten-4', [
      // h('', {style:{'margin':'8px','margin-left':'18px'}}, 'some text'),
      h('div', {style:{'margin':'8px','margin-left':'18px'}}, labels(model, log))
    ]),
  ]);
}

function colorBasedOnTime (date) {
  let hour = moment(date).get('hour');
  return  hour < 2  ? '.blue-grey-text.text-darken-3' :
          hour < 5  ? '.deep-purple-text.text-darken-4' : // purple
          hour < 7  ? '.deep-orange-text.text-darken-4' :
          hour < 10 ? '.blue-text.text-lighten-3' : // light - blue
          hour < 14 ? '.blue-text' : // clear blue
          hour < 17 ? '.blue-text.text-lighten-3' : // lightblur
          hour < 19 ? '.deep-orange-text.text-darken-4' : 
          hour < 22 ? '.deep-purple-text.text-darken-4' : // purple
                      '.blue-grey-text.text-darken-3';
}

function buildLogVM (model, logId) {
  let log = !logId ? null : model.logs[logId];
  return log ? {
    createdAt : moment(log.createdAt).format('h:mm A'),
    message   : log.message,
    duration  : moment.utc(model.time - new Date(log.createdAt)).format('H:mm:ss')
  } : {
    createdAt : null,
    message   : 'You haven\'t logged in anything',
    duration  : '--:--:--'
  };
}

function currentLogView (model) {
  let logVM = buildLogVM(model, model.curLogId);
  return h('div.row', [
    h('h4.col.s12.center', logVM.message),
    h('h1.col.s12.center', {style:{'font-weight':'200', 'font-size': '6rem'}}, logVM.duration),
    h('div.col.s12.center', labels(model, model.logs[model.curLogId], 'main')),
    h('p.col.s12.center', logVM.createdAt ? 
      ['since ', h('b', logVM.createdAt)] :
      []
    )
  ]);
}

function labels (model, log, idPrefix) {
  if (!log) { return []; }

  let taggings = model.taggings[log.id] || [];
  let tags = _.filter(_.map(taggings, tagId => model.tags[tagId]));
  return [
    _.map(tags, _.curry(label)(log)),
    labelInput(log, model.tags, idPrefix)
  ];
}

function label (log, tag) {
  let {id,name} = tag || {id:null,name:''};
  return h('span.z-depth-1', {style:{display:'inline-block', padding:'3px', margin:'2px'}}, [
    name, '   ', h('a#delete-tagging', {attributes:{'data-tag-id':id, 'data-log-id':log.id}}, '\u2717')
  ]);
}

function labelInput (log, tags, idPrefix) {
  var dropdownId = (idPrefix || '') + '-dropdown-' + log.id;
  return h('span', [
    h('a.dropdown-button.teal.lighten-4.z-depth-1', 
      {
        'hook-init': new initDropdownHook(),
        attributes:{'data-activates':dropdownId},
        style:{padding:'5px 10px', margin:'2px'}
      },
      '+'),
    h('ul#' + dropdownId + '.dropdown-content', _.map(tags, (tag) => 
      h('li', [
        h('a#create-tagging', {attributes:{'data-log-id':log.id, 'data-tag-id':tag.id}}, tag.name)
      ])
    ))
  ]);
}

function initDropdownHook () {}
initDropdownHook.prototype.hook = (node) => {
  initDropdown(node);
};

function initDropdown (node) {
  $(node).dropdown({
    inDuration: 300,
    outDuration: 225,
    constrain_width: false,
    gutter: 0, // Spacing from edge
    belowOrigin: false // Displays dropdown below the button
  });
}

//// INPUT

function input (DOM) {
  return {
    createTagging$: parseCreateTagging(DOM),
    removeTagging$: parseRemoveTagging(DOM)
  };
}

function parseCreateTagging (DOM) {
  return DOM.get('#create-tagging', 'click').map(e => {
    let el = $(e.target);
    return {
      logId: el.data('log-id'),
      tagId: el.data('tag-id')
    };
  });
}

function parseRemoveTagging (DOM) {
  return DOM.get('#delete-tagging', 'click').map(e => {
    let el = $(e.target);
    return {
      logId: el.data('log-id'),
      tagId: el.data('tag-id')
    };
  });
}