Feature: Shorts

    Shorts map short identifiers to longer URLs. Shorts are matched by string
    equality, where the identifiers appear in request paths after the prefix
    "/s/". Shorts are stored in a database, which is a JSON file with no more
    than a JSON object in it.

    Scenario: A short matches
        Given the short database at "testdata/exampleshorts.json"
        When I request "/s/abcdef"
        Then I receive a "302" status code
        And I am redirected to "https://example.com"

    Scenario: No short matches
        Given the short database at "testdata/noshorts.json"
        When I request "/s/abcdef"
        Then I receive a "404" status code
