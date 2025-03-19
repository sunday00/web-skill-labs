export const routes = {
  "articles": {
    "path": "/articles",
    "$id": {
      "path": "/articles/:id",
      "_index": {
        "path": "/articles/:id"
      },
      "edit": {
        "path": "/articles/:id/edit"
      }
    },
    "_list": {
      "path": "/articles"
    }
  },
  "auth": {
    "path": "/auth",
    "signin": {
      "path": "/auth/signin"
    },
    "signout": {
      "path": "/auth/signout"
    },
    "signup": {
      "path": "/auth/signup"
    },
    "sns": {
      "path": "/auth/sns",
      "$provider": {
        "path": "/auth/sns/:provider",
        "callback": {
          "path": "/auth/sns/:provider/callback"
        }
      }
    }
  },
  "newsletters": {
    "path": "/newsletters"
  },
  "plain": {
    "path": "/plain"
  }
}