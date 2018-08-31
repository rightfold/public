Given(/^the rule set defined in "([^"]*)"$/) do |rules_path|
    @pid = spawn('urd', rules_path)
    sleep 0.01
end

When(/^I request "([^"]*)"$/) do |request_path|
    @response = Net::HTTP.get_response('127.0.0.1', request_path, 8000)
end

Then(/^I receive a "([^"]*)" status code$/) do |status_code|
    actual = @response.code
    raise "Unexpected response code: #{actual}" if actual != status_code
end

Then(/^I am redirected to "([^"]*)"$/) do |location|
    actual = @response['location']
    raise "Unexpected redirect: #{actual}" if actual != location
end
