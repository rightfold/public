#!/usr/bin/env perl

use strict;
use warnings;

use PN::Ticket;
use Test::More;

plan tests => 4;

{
    open my $fh, '<', "$ENV{TESTDATA}/empty.pnt" or die $!;
    my $ticket = PN::Ticket->parse($fh);
    is_deeply { $ticket->headers }, { };
    is $ticket->body, '';
}

{
    open my $fh, '<', "$ENV{TESTDATA}/troll.pnt" or die $!;
    my $ticket = PN::Ticket->parse($fh);
    is_deeply { $ticket->headers },
              { subject => 'YOUR CODE SUCKS'
              , date    => 'Mon, 4 Sep 2018 13:37:00 +0000' };
    is $ticket->body, "Haha I'm better than you are!\n\nGo away!\n";
}
