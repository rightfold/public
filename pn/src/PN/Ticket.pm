package PN::Ticket;

use strict;
use warnings;

=head2 $ticket->headers
Return the headers of a ticket as a hash of strings.
=cut

sub headers
{
    %{$_[0]->{headers}};
}

=head2 $ticket->body
Return the body of a ticket as a string.
=cut

sub body
{
    $_[0]->{body};
}

=head2 $package->parse($fh)
Parse a ticket from a file handle and return a C<$package>-blessed reference
with the contents of the ticket.
=cut

sub parse
{
    my ($cls, $fh) = @_;

    my %headers;
    while (<$fh>)
    {
        s/^\s*|\s*$//g;
        last if /^$/;
        my ($key, $value) = split /\s*:\s*/, $_, 2;
        $headers{lc($key)} = $value;
    }

    my $body = do { local $/ = undef; <$fh> } // '';

    bless { headers => { %headers }, body => $body }, $cls;
}

1;
