/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* DOM object holding utility CSS functions */

#include "CSS.h"

#include "mozilla/ServoBindings.h"
#include "mozilla/ServoStyleSet.h"
#include "mozilla/dom/BindingDeclarations.h"
#include "mozilla/dom/CSSBinding.h"
#include "mozilla/dom/Document.h"
#include "mozilla/dom/DocumentInlines.h"
#include "nsChangeHint.h"
#include "nsGlobalWindow.h"
#include "nsIURI.h"
#include "nsPresContext.h"
#include "nsStyleUtil.h"
#include "xpcpublic.h"

namespace mozilla {
namespace dom {

/* static */
bool CSS::Supports(const GlobalObject& aGlobal, const nsAString& aProperty,
                   const nsAString& aValue, ErrorResult& aRv) {
  NS_ConvertUTF16toUTF8 property(aProperty);
  NS_ConvertUTF16toUTF8 value(aValue);
  return Servo_CSSSupports2(&property, &value);
}

/* static */
bool CSS::Supports(const GlobalObject& aGlobal, const nsAString& aCondition,
                   ErrorResult& aRv) {
  NS_ConvertUTF16toUTF8 cond(aCondition);
  return Servo_CSSSupports(&cond);
}

/* static */
void CSS::Escape(const GlobalObject& aGlobal, const nsAString& aIdent,
                 nsAString& aReturn) {
  nsStyleUtil::AppendEscapedCSSIdent(aIdent, aReturn);
}

static bool GetDocument(const GlobalObject& aGlobal, Document*& aDocument) {
  nsCOMPtr<nsPIDOMWindowInner> win = do_QueryInterface(aGlobal.GetAsSupports());
  if (!win) {
    return false;
  }

  Document* doc = win->GetDoc();
  if (!doc) {
    return false;
  }

  aDocument = doc;
  return true;
}

static bool RebuildAllStyleData(Document* aDocument) {
  nsPresContext* pcx;
  if ((pcx = aDocument->GetPresContext())) {
    pcx->PostRebuildAllStyleDataEvent(nsChangeHint(0),
                                      RestyleHint::RestyleSubtree());
    return true;
  } else {
    return false;
  }
}

static void HandlePropertyRegistrationResult(
    Document* aDocument, mozilla::PropertyRegistrationResult aResult,
    mozilla::ErrorResult& aRv) {
  switch (aResult) {
    case PropertyRegistrationResult::Ok:
      if (!RebuildAllStyleData(aDocument)) {
        aRv.Throw(NS_ERROR_FAILURE);
      }
      break;
    case PropertyRegistrationResult::SyntaxError:
      aRv.ThrowDOMException(NS_ERROR_DOM_SYNTAX_ERR);
      break;
    case PropertyRegistrationResult::InvalidModificationError:
      aRv.ThrowDOMException(NS_ERROR_DOM_INVALID_MODIFICATION_ERR);
      break;
    case PropertyRegistrationResult::NotFoundError:
      aRv.ThrowDOMException(NS_ERROR_DOM_NOT_FOUND_ERR);
      break;
  }
}

/* static */
void CSS::RegisterProperty(
    const GlobalObject& aGlobal,
    const mozilla::dom::PropertyDescriptorDict& aDescriptor,
    mozilla::ErrorResult& aRv) {
  Document* document = nullptr;
  if (!GetDocument(aGlobal, document)) {
    aRv.Throw(NS_ERROR_FAILURE);
    return;
  }

  nsString maybeInitialValue = EmptyString();
  if (aDescriptor.mInitialValue.WasPassed()) {
    maybeInitialValue = aDescriptor.mInitialValue.Value();
  }

  HandlePropertyRegistrationResult(
      document,
      document->StyleSetForPresShellOrMediaQueryEvaluation()->RegisterProperty(
          aDescriptor.mName, aDescriptor.mSyntax, aDescriptor.mInherits,
          aDescriptor.mInitialValue.WasPassed(), maybeInitialValue),
      aRv);
}

/* static */
void CSS::UnregisterProperty(const GlobalObject& aGlobal,
                             const nsAString& aName,
                             mozilla::ErrorResult& aRv) {
  Document* document = nullptr;
  if (!GetDocument(aGlobal, document)) {
    aRv.Throw(NS_ERROR_FAILURE);
    return;
  }

  HandlePropertyRegistrationResult(
      document,
      document->StyleSetForPresShellOrMediaQueryEvaluation()
          ->UnregisterProperty(aName),
      aRv);
}

}  // namespace dom
}  // namespace mozilla
