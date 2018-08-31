Feature: Rules

    Rules map patterns to redirects. The patterns are matched against the
    paths of requests sent to UR, and the redirects are included in the
    responses. When starting UR, a file must be given that defines one rule
    per line. The pattern and the redirect are separated by a space. The
    first rule that matches will be applied.

    Scenario: A rule matches
        Given the rule set defined in "testdata/examplerules.txt"
        When I request "/com/1234"
        Then I receive a "302" status code
        And I am redirected to "https://example.com/1234"

    Scenario: No rule matches
        Given the rule set defined in "testdata/norules.txt"
        When I request "/com/1234"
        Then I receive a "404" status code
