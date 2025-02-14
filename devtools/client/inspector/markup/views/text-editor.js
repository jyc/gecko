/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

"use strict";

const { editableField } = require("devtools/client/shared/inplace-editor");
const { LocalizationHelper } = require("devtools/shared/l10n");

loader.lazyRequireGetter(this, "getAutocompleteMaxWidth", "devtools/client/inspector/markup/utils", true);
loader.lazyRequireGetter(this, "getLongString", "devtools/client/inspector/shared/utils", true);

const INSPECTOR_L10N =
  new LocalizationHelper("devtools/client/locales/inspector.properties");

/**
 * Creates a simple text editor node, used for TEXT and COMMENT
 * nodes.
 *
 * @param  {MarkupContainer} container
 *         The container owning this editor.
 * @param  {DOMNode} node
 *         The node being edited.
 * @param  {String} type
 *         The type of editor to build. This can be either 'text' or 'comment'.
 */
function TextEditor(container, node, type) {
  this.container = container;
  this.markup = this.container.markup;
  this.node = node;
  this._selected = false;

  this.buildMarkup(type);

  editableField({
    element: this.value,
    stopOnReturn: true,
    trigger: "dblclick",
    multiline: true,
    maxWidth: () => getAutocompleteMaxWidth(this.value, this.container.elt),
    trimOutput: false,
    done: (val, commit) => {
      if (!commit) {
        return;
      }
      getLongString(this.node.getNodeValue()).then(oldValue => {
        this.container.undo.do(() => {
          this.node.setNodeValue(val);
        }, () => {
          this.node.setNodeValue(oldValue);
        });
      });
    },
    cssProperties: this.markup.inspector.cssProperties,
  });

  this.update();
}

TextEditor.prototype = {
  buildMarkup: function(type) {
    const doc = this.markup.doc;

    this.elt = doc.createElement("span");
    this.elt.classList.add("editor", type);

    if (type === "comment") {
      const openComment = doc.createElement("span");
      openComment.textContent = "<!--";
      this.elt.appendChild(openComment);
    }

    this.value = doc.createElement("pre");
    this.value.setAttribute("style", "display:inline-block;white-space: normal;");
    this.value.setAttribute("tabindex", "-1");
    this.elt.appendChild(this.value);

    if (type === "comment") {
      const closeComment = doc.createElement("span");
      closeComment.textContent = "-->";
      this.elt.appendChild(closeComment);
    }
  },

  get selected() {
    return this._selected;
  },

  set selected(value) {
    if (value === this._selected) {
      return;
    }
    this._selected = value;
    this.update();
  },

  update: function() {
    getLongString(this.node.getNodeValue()).then(str => {
      this.value.textContent = str;

      const isWhitespace = !/[^\s]/.exec(str);
      this.value.classList.toggle("whitespace", isWhitespace);

      const chars = str.replace(/\n/g, "⏎")
                     .replace(/\t/g, "⇥")
                     .replace(/ /g, "◦");
      this.value.setAttribute("title", isWhitespace
        ? INSPECTOR_L10N.getFormatStr("markupView.whitespaceOnly", chars)
        : "");
    }).catch(console.error);
  },

  destroy: function() {},

  /**
   * Stub method for consistency with ElementEditor.
   */
  getInfoAtNode: function() {
    return null;
  },
};

module.exports = TextEditor;
