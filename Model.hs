--  eidolon -- A simple gallery in Haskell and Yesod
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Model where

import ClassyPrelude.Yesod
import Yesod.Markdown (Markdown, unMarkdown)
import Database.Persist.Quasi
import qualified System.FilePath as FP

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data ESInput = ESUser UserId User
  | ESAlbum AlbumId Album
  | ESMedium MediumId Medium
  | ESComment CommentId Comment

data FullUser = FullUser
  {userRep :: User}

instance ToJSON User where
  toJSON (User name slug _ _ _ _ _) =
    object
      [ "name" .= name
      , "slug" .= slug
      ]

instance ToJSON FullUser where
  toJSON (FullUser user) =
    object
      [ "name" .= (userName user)
      , "slug" .=  (userSlug user)
      , "albums" .= (userAlbums user)
      ]

data FullAlbum = FullAlbum
  {albumRep :: Album}

instance ToJSON Album where
  toJSON (Album title _ _ _ _ _) =
    object
      [ "name" .= title ]

instance ToJSON FullAlbum where
  toJSON (FullAlbum album) =
    object
      [ "name" .= (albumTitle album)
      , "owner" .= (albumOwner album)
      , "shares" .= (albumShares album)
      , "content" .= (albumContent album)
      ]

data FullMedium = FullMedium
  {mediumRep :: Medium} 

instance ToJSON Medium where
  toJSON (Medium title _ _ _ time _ desc tags _ _ _ _ _) =
    object
      [ "name" .= title
      , "time" .= time
      , "description" .= desc
      , "tags" .= tags
      ]

instance ToJSON FullMedium where
  toJSON (FullMedium medium) =
    object
      [ "name" .= (mediumTitle medium)
      , "time" .= (mediumTime medium)
      , "owner" .= (mediumOwner medium)
      , "description" .= (mediumDescription medium)
      , "tage" .= (mediumTags medium)
      , "album" .= (mediumAlbum medium)
      ]

data FullComment = FullComment
  {commentRep :: Comment}

instance ToJSON Comment where
  toJSON (Comment _ slug _ _ time cont) =
    object
      [ "author" .= slug
      , "time" .= time
      , "content" .= (unMarkdown cont)
      ]

instance ToJSON FullComment where
  toJSON (FullComment comment) =
    object
      [ "author_id" .= (commentAuthor comment)
      , "author" .= (commentAuthorSlug comment)
      , "origin" .= (commentOrigin comment)
      , "parent" .= (commentParent comment)
      , "time" .= (commentTime comment)
      , "content" .= (unMarkdown $ commentContent comment)
      ]
