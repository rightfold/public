Feature: Shorts

    Shorts map short identifiers to redirects. The identifiers are matched
    against the paths of requests sent to UR, and the redirects are included
    in the responses. When starting UR, a file must be given that defines one
    short per line. The short and the redirect are separated by a space. UR
    will append to this file when a new short is created through the web
    interface.

    Scenario: A short matches
        Given the short database at "testdata/exampleshorts.txt"
        When I request "/s/abcdef"
        Then I receive a "302" status code
        And I am redirected to "https://example.com"

    Scenario: No short matches
        Given the short database at "testdata/noshorts.txt"
        When I request "/s/abcdef"
        Then I receive a "404" status code
