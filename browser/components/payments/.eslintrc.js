"use strict";

module.exports = {
  overrides: [
    {
      files: [
        "res/components/*.js",
        "res/containers/*.js",
        "res/mixins/*.js",
        "res/paymentRequest.js",
        "res/PaymentsStore.js",
        "test/mochitest/test_*.html",
      ],
      parserOptions: {
        sourceType: "module",
      },
    },
    {
      "files": "test/unit/head.js",
      "rules": {
        "no-unused-vars": ["error", {
          "args": "none",
          "vars": "local",
        }],
      },
    },
  ],
  rules: {
    "mozilla/var-only-at-top-level": "error",

    "array-bracket-spacing": ["error", "never"],
    "block-scoped-var": "error",
    complexity: ["error", {
      max: 20,
    }],
    curly: ["error", "all"],
    "dot-location": ["error", "property"],
    "indent-legacy": ["error", 2, {
      SwitchCase: 1,
      CallExpression: {
        arguments: "first",
      },
      FunctionExpression: {
        parameters: "first",
      },
      FunctionDeclaration: {
        parameters: "first",
      },
      // XXX: following line is used in eslint v4 to not throw an error when chaining methods
      //MemberExpression: "off",
      outerIIFEBody: 0,
    }],
    "max-len": ["error", 100],
    "max-nested-callbacks": ["error", 4],
    "new-parens": "error",
    "no-console": ["error", { allow: ["error"] }],
    "no-fallthrough": "error",
    "no-multi-str": "error",
    "no-multiple-empty-lines": ["error", {
      max: 2,
    }],
    "no-proto": "error",
    "no-unused-expressions": "error",
    "no-unused-vars": ["error", {
      args: "none",
      vars: "all"
    }],
    "no-use-before-define": ["error", {
      functions: false,
    }],
    radix: "error",
    "semi-spacing": ["error", {"before": false, "after": true}],
    "space-in-parens": ["error", "never"],
    "valid-jsdoc": ["error", {
      prefer: {
        return: "returns",
      },
      preferType: {
        Boolean: "boolean",
        Number: "number",
        String: "string",
        bool: "boolean",
      },
      requireParamDescription: false,
      requireReturn: false,
      requireReturnDescription: false,
    }],
    yoda: "error",
  },
};
