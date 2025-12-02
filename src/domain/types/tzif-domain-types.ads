pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Types
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for domain utility types.
--    Contains generic Option and Bounded_Vector implementations
--    that are cloned from Functional crate for domain layer purity.
--
--  Child Packages:
--    TZif.Domain.Types.Option         - Optional value container
--    TZif.Domain.Types.Bounded_Vector - SPARK-friendly bounded container
--
--  Design:
--    Domain layer must have ZERO external crate dependencies.
--    These are minimal clones for Domain use only.
--    Infrastructure/Application should use Functional crate equivalents.
--
--  ===========================================================================

package TZif.Domain.Types with
  Pure
is
   --  This is a parent package for domain utility types.
   --  See child packages for actual type definitions.
end TZif.Domain.Types;
