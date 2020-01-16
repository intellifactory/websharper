// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

module WebSharper.Tests.DateTime

open WebSharper
open WebSharper.JavaScript
open WebSharper.Testing

type private DayOfWeek    = System.DayOfWeek
type private DateTime     = System.DateTime
type private DateTimeKind = System.DateTimeKind
type private TimeSpan     = System.TimeSpan

[<JavaScript>]
let private d = DateTime(2010, 4, 8, 15, 5, 39)

[<JavaScript>]
let Tests =

    TestCategory "DateTime" {

        Test "Year" {
            equal d.Year 2010
        }

        Test "Month" {
            equal d.Month 4
        }

        Test "Day" {
            equal d.Day 8
        }

        Test "Hour" {
            equal d.Hour 15
        }

        Test "Minute" {
            equal d.Minute 5
        }

        Test "Second" {
            equal d.Second 39
        }

        Test "Millisecond" {
            equal d.Millisecond 0
        }

        Test "DayOfWeek" {
            equal d.DayOfWeek DayOfWeek.Thursday
        }

        Test "Add" {
            equal (d.Add(TimeSpan(3, 4, 15)))
                (DateTime(2010, 4, 8, 18, 9, 54))
            equal (d + TimeSpan(3, 4, 15))
                (DateTime(2010, 4, 8, 18, 9, 54))
        }

        Test "Subtract" {
            equal (d.Subtract(TimeSpan(3, 4, 15)))
                (DateTime(2010, 4, 8, 12, 1, 24))
            equal (d.Subtract(DateTime(2010, 4, 8, 12, 1, 24)))
                (TimeSpan(3, 4, 15))
            equal (d - TimeSpan(3, 4, 15))
                (DateTime(2010, 4, 8, 12, 1, 24))
            equal (d - DateTime(2010, 4, 8, 12, 1, 24))
                (TimeSpan(3, 4, 15))
        }

        Test "AddYears" {
            equal (d.AddYears 24) (DateTime(2034, 4, 8, 15, 5, 39))
        }

        Test "AddMonths" {
            equal (d.AddMonths 9) (DateTime(2011, 1, 8, 15, 5, 39))
        }

        Test "AddDays" {
            equal (d.AddDays 2.3) (DateTime(2010, 4, 10, 22, 17, 39))
        }

        Test "AddHours" {
            equal (d.AddHours 1.05) (DateTime(2010, 4, 8, 16, 08, 39))
        }

        Test "AddMinutes" {
            equal (d.AddMinutes 141.23) (DateTime(2010, 4, 8, 17, 26, 52, 800))
        }

        Test "AddSeconds" {
            equal (d.AddSeconds 234.15) (DateTime(2010, 4, 8, 15, 09, 33, 150))
        }

        Test "AddMilliseconds" {
            equal (d.AddMilliseconds 123.) (DateTime(2010, 4, 8, 15, 05, 39, 123))
        }

        Test "AddTicks" {
            equal (d.AddTicks 1230000L) (DateTime(2010, 4, 8, 15, 05, 39, 123))
        }

        Test "Date" {
            approxEqual (As<float> d.Date) (As<float> (DateTime(2010, 4, 8)))
        }

        Test "TimeOfDay" {
            approxEqual (As<float> d.TimeOfDay) (As<float> (TimeSpan(15, 5, 39)))
        }

        Test "Ticks" {
            equal d (DateTime(d.Ticks))
            equal d.Ticks (DateTime(1970, 1, 1, 0, 0, 0).Ticks + As<int64> d * 10_000L)
            equal (d.AddTicks(123456789L).Ticks) (d.Ticks + 123456789L)
        }

    //    Test "Kind" {
    //        DateTime.Now.Kind =? DateTimeKind.Local
    //        DateTime.UtcNow.Kind =? DateTimeKind.Utc
    //        d.Kind =? DateTimeKind.Unspecified
    //    }

    //    Test "SpecifyKind" {
    //        DateTime.SpecifyKind(d, DateTimeKind.Utc).Kind =? DateTimeKind.Utc
    //        DateTime.SpecifyKind(d, DateTimeKind.Local).Kind =? DateTimeKind.Local
    //    }

    //    Test "ToLocalTime" {
    //        DateTime.UtcNow.ToLocalTime() =? DateTime.Now
    //    }
    //
    //    Test "ToUtcTime" {
    //        DateTime.Now.ToUniversalTime() =? DateTime.UtcNow
    //    }

        Test "Comparison" {
            let d1 = DateTime(2010, 4, 8, 15, 5, 39)
            let d2 = DateTime(2010, 4, 8, 15, 5, 40)
            equal (compare d d1) 0
            equal (compare d d2) -1
            equal (compare d2 d) 1
            isTrue (d1 < d2)
            isTrue (d <= d1)
        }

        Test "Equality" {
            let d1 = DateTime(2010, 4, 8, 15, 5, 39)
            let d2 = DateTime(2010, 4, 8, 15, 5, 42)
            equal d d1
            notEqual d d2
            equal (d.GetHashCode()) (d1.GetHashCode())
            notEqual (d.GetHashCode()) (d2.GetHashCode())
        }

    //    Test "Now" {
    //        DateTime.Now.Kind =? DateTimeKind.Local
    //    }
    //
    //    Test "UtcNow" {
    //        DateTime.UtcNow.Kind =? DateTimeKind.Utc
    //    }

        Test "Today" {
            equal DateTime.Today DateTime.Now.Date
        }

        Test "String" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            equal (d.ToShortTimeString()) "15:05"
            equal (d.ToLongTimeString()) "15:05:39"
        }

        Test "Parse" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            raises (DateTime.Parse("not a date"))
            equal (DateTime.Parse("Thu, 08 Apr 2010 15:05:39 GMT")) d
            equal (Date.Parse("Thu, 08 Apr 2010 15:05:39 GMT")) (As<int> d)
        }

        Test "TryParse" {
            let d = DateTime(2010, 4, 8, 15, 5, 39)
            equal (DateTime.TryParse("not a date") |> fst) false
            equal (DateTime.TryParse("Thu, 08 Apr 2010 15:05:39 GMT")) (true, d)
        }

        Test "DaysInMonth" {
            equal (DateTime.DaysInMonth(2018, 6)) 30
            equal (DateTime.DaysInMonth(2018, 2)) 28
            equal (DateTime.DaysInMonth(2020, 2)) 29
        }

        Test "IsLeapYear" {
            isFalse (DateTime.IsLeapYear 2018)
            isTrue (DateTime.IsLeapYear 2020)
            isFalse (DateTime.IsLeapYear 2100)
        }
    }

[<JavaScript>]
let NativeTests =

    TestCategory "Native Dates" {

        Test "Turnaround" {
            approxEqual (As<float> d.JS.Self) (As<float> d)
        }

        Test "Equality" {
            let a = DateTime.Now.JS
            let b = DateTime.Now.JS
            equal a b
            let c = DateTime.Now.AddDays(1.).JS
            notEqual a c
        }

        Test "Comparison" {
            let a = DateTime.Now.JS
            let b = DateTime.Now.AddDays(1.).JS
            isTrue (Unchecked.compare a b < 0)
            isTrue (Unchecked.compare b a > 0)
        }

    }
