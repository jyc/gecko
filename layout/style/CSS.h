/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* DOM object holding utility CSS functions */

#ifndef mozilla_dom_CSS_h_
#define mozilla_dom_CSS_h_

#include "mozilla/Attributes.h"
#include "mozilla/Preferences.h"

namespace mozilla {

class ErrorResult;

namespace dom {

class GlobalObject;
struct PropertyDescriptorDict;

class CSS {
 private:
  CSS() = delete;

 public:
  static bool Supports(const GlobalObject& aGlobal, const nsAString& aProperty,
                       const nsAString& aValue, ErrorResult& aRv);

  static bool Supports(const GlobalObject& aGlobal,
                       const nsAString& aDeclaration, ErrorResult& aRv);

  static void Escape(const GlobalObject& aGlobal, const nsAString& aIdent,
                     nsAString& aReturn);

  static void RegisterProperty(const GlobalObject& aGlobal,
                               const PropertyDescriptorDict& aDescriptor,
                               ErrorResult& aRv);

  static void UnregisterProperty(const GlobalObject& aGlobal,
                                 const nsAString& aName, ErrorResult& aRv);
};

}  // namespace dom
}  // namespace mozilla

#endif  // mozilla_dom_CSS_h_
