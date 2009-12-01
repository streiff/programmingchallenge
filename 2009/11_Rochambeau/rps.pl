#!/usr/bin/perl

use Data::Dumper qw/Dumper/;

use strict;
use warnings;

use constant WINDOW_SIZES => (1, 5, 10, 25, 50, 75, 100);
use constant WINDOW_COEFF => { 100 => 1.00, 
                                75 => 0.95, 
                                50 => 0.90, 
                                25 => 0.80, 
                                10 => 0.75, 
                                5  => 0.70, 
                                1  => 0.50 };
use constant LOSS_MOVES => { "S" => "R", "P" => "S", "R" => "P"}; 
use constant SZ => 100;
use constant DATA_FILE => "dt_ron.dd";
use constant PI_FILENAME => "num.txt";

#####################################################################
sub readStats {
  local $/ = undef;
  open (FILE, "<" . DATA_FILE) || return {};
  my $data = <FILE>;
  close (FILE);
  return eval($data);
}

#####################################################################
sub writeStats {
  open(FILE, ">" . DATA_FILE);
  local $Data::Dumper::Terse = 1;
  local $Data::Dumper::Useqq = 1;
  print FILE Dumper(shift);
  close(FILE);
}

#####################################################################
sub doGuess {
    my $stats = shift;
    my @guesses;

    push @guesses, {guess => piGuess(scalar(@{$stats})), chance => (1/3), size => 0};
    foreach my $size (WINDOW_SIZES) {
        my $guess = predict($stats, $size);
        if (defined($guess)) {
            $guess->{chance} = $guess->{chance} * WINDOW_COEFF->{$size};
            push @guesses, $guess;
        }
    }

    (sort {$b->{chance} <=> $a->{chance} || $b->{size} <=> $a->{size}} @guesses) [0]->{guess};
}

#####################################################################
sub predict {
    my ($stats, $size) = @_;

    return if scalar(@{$stats}) < $size + 1;

    my %predictions;
    my $num_predictions = 0;

    my @lastEntries = @{$stats}[(scalar(@{$stats}) - $size)..(scalar(@{$stats}) - 1)];
    for (my $i = scalar(@{$stats}) - $size - 1; $i >= 0 && $i > scalar(@${stats}) - ($size * SZ); --$i) {
        my @compEntries = @{$stats}[$i..($i+$size-1)];
        if (listEq(\@lastEntries, \@compEntries)) {
            ++$num_predictions;
            my $guess = $stats->[$i + $size][1];
            if (exists($predictions{$guess})) {
                $predictions{$guess} += 1;
            } else {
                $predictions{$guess}  = 1;
            }
        }
    }

    if (scalar(keys %predictions == 0)) {
        return;
    } elsif (scalar(keys %predictions == 1)) {
        return {guess => LOSS_MOVES->{(keys %predictions)[0]}, chance => 1, size => $size};
    } else {
        my $largest_key = "";
        my $largest_value = -1;
        while (my ($key, $value) = each(%predictions)) {
            if ($value > $largest_value) {
                $largest_value = $value;
                $largest_key = $key;
            }
        }
        my $chance = $largest_value / $num_predictions;
        return {guess => LOSS_MOVES->{$largest_key}, chance => $chance, size => $size};
    }
    return;
}

#####################################################################
sub addLastStat {
    my $stats = shift;
    my $opponent = shift;
    my $opp_guess = shift;
    die "I didn't start the game yet" unless $stats->{$opponent};

    my $last_game_pos = scalar(@{$stats->{$opponent}}) - 1;
    if (scalar(@{$stats->{$opponent}[$last_game_pos]}) == 2) {
        die "last game already has an ending move";
    }

    $stats->{$opponent}[$last_game_pos][1] = $opp_guess;
}

#####################################################################
sub addCurrentStat {
    my $stats = shift;
    my $opponent = shift;
    my $guess = shift;

    $stats->{$opponent} = () unless $stats->{$opponent};
    my $last_game_pos = scalar(@{$stats->{$opponent}}) - 1;

    if ($last_game_pos >= 0 && scalar(@{$stats->{$opponent}[$last_game_pos]}) == 1) {
        die "last game not finished yet";
    }
  
    push @{$stats->{$opponent}}, [$guess];
}

#####################################################################
sub listEq {
    my $l1 = shift;
    my $l2 = shift;

    if (scalar(@$l1) != scalar(@$l2)) {
        return 0;
    }

    for (my $i = 0; $i < scalar(@$l1); ++$i) {
        return 0 if $l1->[$i][0] ne $l2->[$i][0];
        return 0 if $l1->[$i][1] ne $l2->[$i][1];
    }
    return 1;
}

#####################################################################
sub piGuess {
  my $num = 3.1415926535;
  my $try = 0;
  if (open (NUM_FILE, "<" . PI_FILENAME)) {
    $num = <NUM_FILE> || 3.1415926535;
    $try = <NUM_FILE> || 0;
    close NUM_FILE;
  }
  chomp($num);
  chomp($try);
  $num =~ s/\D|9//g;

  if ($try >= length($num)) {
    $try = 0;
    $num *= 2;
  }
  my @nums = split("", $num);
  my $index = $nums[$try] % 3;

  $try++;
  open (NUM_FILE, ">" . PI_FILENAME);
  print NUM_FILE "$num\n";
  print NUM_FILE "$try\n";
  close NUM_FILE;
  
  my @arr = ("R", "P", "S");
  $arr[$index];
}

#####################################################################
die "Invalid args" if (scalar(@ARGV) < 2);
my $opponent = $ARGV[0];
my $mode = $ARGV[1];
my $last_move = ($mode eq "turn" || $mode eq "finish") ? $ARGV[2] : undef;
my $stats = readStats();

$stats->{$opponent} = [] unless $stats->{$opponent};

if ($mode eq "turn" || $mode eq "finish") {
    addLastStat($stats, $opponent, $last_move);
}

if ($mode ne "finish") {
    my $guess = doGuess($stats->{$opponent});
    addCurrentStat($stats, $opponent, $guess);
    print $guess, "\n";
}

writeStats($stats);
