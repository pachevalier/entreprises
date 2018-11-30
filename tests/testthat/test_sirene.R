library("entreprises")
context(desc = "Testing entreprises")

test_that(
  desc = "get_company returns a list",
  code = expect_is(
    object = get_company(siret = "38235772100028"),
    class = "list"
    )
  )

test_that(
  desc = "get_url returns a string",
  code = expect_is(
    object = get_url(siret = "38235772100028"),
    class = "character"
  )
)

test_that(
  desc = "search_company returns a list",
  code = expect_is(
    object = search_company(string = "dataiku"),
    class = "list"
  )
)
