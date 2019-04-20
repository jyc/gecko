/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* sets of CSSProperty values (custom or standard properties) */

#ifndef mozilla_CSSPropertySet_h
#define mozilla_CSSPropertySet_h

#include "mozilla/CSSProperty.h"
#include "nsCSSPropertyID.h"
#include "nsCSSPropertyIDSet.h"
#include "nsTHashtable.h"
#include "nsHashKeys.h"
#include "nsIAtom.h"

namespace mozilla {

/**
 * The same as nsCSSPropertyIDSet, but also capable of holding custom
 * properties.
 */
class CSSPropertySet {
public:
  typedef nsTHashtable<nsRefPtrHashKey<nsIAtom>> Table;

  CSSPropertySet() = default;
  CSSPropertySet(const CSSPropertySet& aOther);

  explicit CSSPropertySet(const nsCSSPropertyIDSet& aOther)
    : mStandardProps(aOther)
  { }

  void AddProperty(CSSProperty aProperty);
  void RemoveProperty(CSSProperty aProperty);
  bool HasProperty(CSSProperty aProperty) const;

  const nsCSSPropertyIDSet& StandardProperties() const;

  void Empty();

  bool Equals(const CSSPropertySet& aOther) const;

  Table::Iterator IterCustomProps() const;

private:
  nsCSSPropertyIDSet mStandardProps;
  Table mCustomProps;
};

} // namespace mozilla

#endif // mozilla_CSSPropertySet_h
