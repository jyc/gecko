/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "mozilla/CSSPropertySet.h"
#include "mozilla/Move.h"
#include "mozilla/PodOperations.h"

namespace mozilla {

CSSPropertySet::CSSPropertySet(const CSSPropertySet& aOther)
  : mStandardProps(aOther.mStandardProps)
{
  for (auto iter = aOther.mCustomProps.ConstIter(); !iter.Done(); iter.Next()) {
    mCustomProps.PutEntry(iter.Get()->GetKey());
  }
}

void
CSSPropertySet::AddProperty(CSSProperty aProperty)
{
  if (aProperty.IsStandard()) {
    mStandardProps.AddProperty(aProperty.AsStandard());
  } else {
    nsIAtom* custom = aProperty.AsCustom();
    mCustomProps.PutEntry(custom);
  }
}

void
CSSPropertySet::RemoveProperty(CSSProperty aProperty)
{
  if (aProperty.IsStandard()) {
    mStandardProps.RemoveProperty(aProperty.AsStandard());
  } else {
    nsIAtom* custom = aProperty.AsCustom();
    mCustomProps.RemoveEntry(custom);
  }
}

bool
CSSPropertySet::HasProperty(CSSProperty aProperty) const
{
  if (aProperty.IsStandard()) {
    return mStandardProps.HasProperty(aProperty.AsStandard());
  } else {
    nsIAtom* custom = aProperty.AsCustom();
    return mCustomProps.Contains(custom);
  }
}

const nsCSSPropertyIDSet&
CSSPropertySet::StandardProperties() const
{
  return mStandardProps;
}

void
CSSPropertySet::Empty()
{
  mStandardProps.Empty();
  mCustomProps.Clear();
}

bool
CSSPropertySet::Equals(const CSSPropertySet& aOther) const
{
  if (!mStandardProps.Equals(aOther.mStandardProps) ||
      mCustomProps.Count() != aOther.mCustomProps.Count()) {
    return false;
  }
  for (auto iter = mCustomProps.ConstIter(); !iter.Done(); iter.Next()) {
    if (!aOther.mCustomProps.GetEntry(iter.Get()->GetKey())) {
      return false;
    }
  }
  return true;
}

CSSPropertySet::Table::Iterator
CSSPropertySet::IterCustomProps() const
{
  return mCustomProps.ConstIter();
}

} // naemspace mozilla
