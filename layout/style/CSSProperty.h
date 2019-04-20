/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* type for representing CSS properties (custom or standard) */
 
#ifndef mozilla_CSSProperty_h
#define mozilla_CSSProperty_h

#include "mozilla/AlreadyAddRefed.h"
#include "mozilla/RefPtr.h"
#include "nsCSSPropertyID.h"
#include "nsCSSProps.h"
#include "nsAtom.h"
#include "nsStyleStructFwd.h"

namespace mozilla {

/**
 * A CSSProperty represents either a registered custom property or a standard
 * CSS property. IsStandard() and AsStandard() check for and retrieve standard
 * CSS properties, and IsCustom() and AsCustom() do the same for custom
 * properties. Custom properties are encoded as atoms containing their names
 * without the leading '--', while standard properties are represented by
 * nsCSSPropertyID.
 */
class CSSProperty
{
private:
  enum State : uint8_t {
    Invalid,
    Standard,
    Custom,
  };

public:
  CSSProperty()
    : mState(State::Invalid)
  {
  }

  explicit CSSProperty(nsCSSPropertyID aProperty)
    : mState(State::Standard)
  {
    mValue.mStandard = aProperty;
  }

  explicit CSSProperty(nsAtom* aProperty)
    : mState(State::Custom)
  {
    mValue.mCustom = aProperty;
    mValue.mCustom->AddRef();
  }

  explicit CSSProperty(already_AddRefed<nsAtom> aProperty)
    : mState(State::Custom)
  {
    mValue.mCustom = aProperty.take();
  }

  CSSProperty(const CSSProperty& aOther)
    : mState(aOther.mState)
  {
    if (mState == State::Custom) {
      mValue.mCustom = aOther.mValue.mCustom;
      mValue.mCustom->AddRef();
    } else {
      mValue.mStandard = aOther.mValue.mStandard;
    }
  }

  CSSProperty(CSSProperty&& aOther)
    : mState(State::Invalid)
  {
    *this = aOther;
  }

  CSSProperty& operator=(const CSSProperty& aOther)
  {
    CSSProperty copy(aOther);
    *this = std::move(copy);
    return *this;
  }

  CSSProperty& operator=(CSSProperty&& aOther)
  {
    if (mState == State::Custom) {
      mValue.mCustom->Release();
    }
    mState = aOther.mState;
    if (mState == State::Custom) {
      mValue.mCustom = aOther.mValue.mCustom;
      // Don't want double-frees.
      aOther.mValue.mCustom = nullptr;
      aOther.mState = State::Invalid;
    } else {
      mValue.mStandard = aOther.mValue.mStandard;
    }
    return *this;
  }

  bool operator==(const CSSProperty& aOther) const
  {
    MOZ_ASSERT(mState != State::Invalid &&
               aOther.mState != State::Invalid);
    if (mState == State::Custom) {
      return aOther.mState == State::Custom &&
             mValue.mCustom == aOther.mValue.mCustom;
    } else {
      return mValue.mStandard == aOther.mValue.mStandard;
    }
  }

  bool operator!=(const CSSProperty& aOther) const
  {
    return !(*this == aOther);
  }

  bool operator==(const nsCSSPropertyID& aOther) const
  {
    MOZ_ASSERT(mState != State::Invalid);
    return mState == State::Standard &&
           mValue.mStandard == aOther;
  }

  bool operator!=(const nsCSSPropertyID& aOther) const
  {
    return !(*this == aOther);
  }

  bool operator==(nsAtom* aOther) const
  {
    MOZ_ASSERT(mState != State::Invalid);
    return mState == State::Custom &&
           mValue.mCustom == aOther;
  }

  bool operator!=(nsAtom* aOther) const
  {
    return !(*this == aOther);
  }

  ~CSSProperty()
  {
    if (mState == State::Custom) {
      mValue.mCustom->Release();
    }
  }

  bool IsStandard() const
  {
    MOZ_ASSERT(mState != State::Invalid);
    return mState == State::Standard;
  }

  bool IsCustom() const
  {
    MOZ_ASSERT(mState != State::Invalid);
    return mState == State::Custom;
  }

  nsCSSPropertyID AsStandard() const
  {
    MOZ_ASSERT(mState == State::Standard);
    return mValue.mStandard;
  }

  nsAtom* AsCustom() const
  {
    MOZ_ASSERT(mState == State::Custom);
    return mValue.mCustom;
  }

  ///**
  // * GetStyleStructID returns the nsStyleStructID corresponding to this
  // * property. Custom properties always have style struct ID
  // * eStyleStruct_Variables.
  // */
  //StyleStructID GetStyleStructID() const
  //{
  //  switch (mState) {
  //    case State::Invalid:
  //      break;
  //    case State::Standard:
  //      return nsCSSProps::kSIDTable[mStandard];
  //    case State::Custom:
  //      return eStyleStruct_Variables;
  //  }
  //  MOZ_ASSERT(false);
  //  return 0;
  //}

  /**
   * ToString converts this property into its CSS property name.
   * Custom properties have '--' prepended.
   */
  void ToString(nsAString& aString) const
  {
    aString.Truncate(0);
    switch (mState) {
      case State::Invalid:
        MOZ_ASSERT(false);
        break;
      case State::Standard:
        aString = NS_ConvertASCIItoUTF16(nsCSSProps::GetStringValue(mValue.mStandard));
        break;
      case State::Custom:
        nsAutoString name;
        mValue.mCustom->ToString(name);
        aString.AppendLiteral("--");
        aString.Append(name);
        break;
    }
  }

  /**
   * IsShorthand returns true iff this represents a standard shorthand property.
   * Custom properties are never shorthands.
   */
  bool IsShorthand() const
  {
    MOZ_ASSERT(mState != State::Invalid);
    if (mState == State::Custom) {
      return false;
    }
    return nsCSSProps::IsShorthand(mValue.mStandard);
  }

  ///**
  // * ValueRestrictions(x) returns nsCSSProps::ValueRestrictions(x) if x is a
  // * standard property and 0 if x is a cusotm property.
  // */
  //uint32_t ValueRestrictions() const
  //{
  //  switch (mState) {
  //    case State::Invalid:
  //      MOZ_ASSERT(false);
  //      break;
  //    case State::Standard:
  //      return nsCSSProps::ValueRestrictions(mStandard);
  //    case State::Custom:
  //      return 0;
  //  }
  //}

  /**
   * HasFlags(x) returns nsCSSProps::PropHasFlags(x) if x is a standard
   * property and false if x is a custom property.
   */
  bool HasFlags(nsCSSProps::Flags aFlags) const {
    return mState == State::Standard &&
           nsCSSProps::PropHasFlags(mValue.mStandard, aFlags);
  }

  /**
   * IsCustomLessThan returns true iff this and aOther are custom properties
   * and the custom property name for this property is less than (according to
   * nsString::operator<) aOther's custom property name. It is an error to call
   * IsCustomLessThan if this or aOther do not represent custom properties.
   */
  bool IsCustomLessThan(const CSSProperty& aOther) const
  {
    MOZ_ASSERT(mState == State::Custom &&
               aOther.mState == State::Custom);
    nsString left;
    nsString right;
    mValue.mCustom->ToString(left);
    aOther.mValue.mCustom->ToString(right);
    return left < right;
  }

private:
  State mState;
  union {
    nsCSSPropertyID mStandard;
    nsAtom* mCustom;
  } mValue;
};

} // namespace mozilla

#endif // mozilla_CSSProperty_h
