module Session exposing (..)

--A session has a unique number so that users can
--open multiple tabs with the same avatar and composition.

--Sessions persist indirectly through their commits.

--A Modification is always originating from a Session.
