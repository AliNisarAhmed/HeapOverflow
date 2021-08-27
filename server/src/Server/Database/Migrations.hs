module Server.Database.Migrations where

import Data.Time (getCurrentTime)
import Database.Persist (LiteralType (DbSpecific))
import Database.Persist.Migration
import Database.Persist.Migration.Operation
import RIO

migration :: Migration
migration =
  [ 0 ~> 1 := [createUserTable, createQuestionTable, createAnswerTable]
  ]

createUserTable :: Operation
createUserTable =
  CreateTable
    { name = "user",
      schema =
        [ Column "id" SqlInt32 [NotNull, AutoIncrement],
          Column "firstname" SqlString [NotNull],
          Column "surname" SqlString [NotNull],
          Column "username" SqlString [NotNull],
          Column "email" SqlString [NotNull],
          Column "pwd" SqlString [NotNull],
          Column "salt" SqlString [NotNull]
        ],
      constraints =
        [ PrimaryKey ["id"],
          Unique "unique_user_fullname" ["firstname", "surname"],
          Unique "unique_user_email" ["email"],
          Unique "unique_username" ["username"]
        ]
    }

createQuestionTable :: Operation
createQuestionTable =
  CreateTable
    { name = "question",
      schema =
        [ Column "id" SqlInt32 [NotNull, AutoIncrement],
          Column "title" SqlString [NotNull],
          Column "content" SqlString [NotNull],
          Column "user_id" SqlInt32 [References ("user", "id")],
          Column "created_at" SqlDayTime [NotNull, Default $ PersistLiteral "now()"],
          Column "updated_at" SqlDayTime [NotNull, Default $ PersistLiteral "now()"]
        ],
      constraints =
        [ PrimaryKey ["id"]
        ]
    }

createAnswerTable :: Operation
createAnswerTable =
  CreateTable
    { name = "answer",
      schema =
        [ Column "id" SqlInt32 [NotNull, AutoIncrement],
          Column "content" SqlString [NotNull],
          Column "question_id" SqlInt32 [References ("question", "id")],
          Column "author_id" SqlInt32 [References ("user", "id")],
          Column "created_at" SqlDayTime [NotNull, Default $ PersistLiteral "now()"],
          Column "updated_at" SqlDayTime [NotNull, Default $ PersistLiteral "now()"]
        ],
      constraints =
        [ PrimaryKey ["id"]
        ]
    }
