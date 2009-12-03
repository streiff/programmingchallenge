#!/usr/bin/perl

use Data::Dumper qw/Dumper/;

use strict;
use warnings;

use constant WINDOW_SIZES => (1, 5, 10, 25, 50, 75, 100);
use constant WINDOW_COEFF => { 100 => 1.00, 
                                75 => 0.98, 
                                50 => 0.94, 
                                25 => 0.90, 
                                10 => 0.80, 
                                5  => 0.70, 
                                1  => 0.60 };
use constant LOSS_MOVES => { "S" => "R", "P" => "S", "R" => "P"}; 
use constant SZ => 75;
use constant TMP_DIR => "/tmp/ronrps";
use constant DATA_FILE => "/tmp/ronrps/dt_";
use constant PI_FILENAME => "/tmp/ronrps/num.txt";

#####################################################################
sub readStats {
  my $opp = shift;  
  open (FILE, "<" . DATA_FILE . $opp) || return ();
  my @stats;
  while (<FILE>) {
    chomp;
    my @game =  split("", $_);
    push @stats, \@game;
  }
  close (FILE);
  return \@stats;
}

#####################################################################
sub writeStats {
  my $stats = shift;
  my $opp = shift;  

  open(FILE, ">" . DATA_FILE . $opp);
  foreach my $game (@$stats) {
    print FILE join("", @$game), "\n";
  }
  close(FILE);
}

#####################################################################
sub doGuess {
    my $stats = shift;
    my @guesses;

    push @guesses, {guess => piGuess(scalar(@{$stats})), chance => (1/3*0.87), size => 0};
    foreach my $size (WINDOW_SIZES) {
        next if ($size < 10 && scalar(@$stats) > 100);
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
    my $opp_guess = shift;

    my $last_game_pos = scalar(@{$stats}) - 1;
    if (scalar(@{$stats}[$last_game_pos]) == 2) {
        die "last game already has an ending move";
    }

    $stats->[$last_game_pos][1] = $opp_guess;
}

#####################################################################
sub addCurrentStat {
    my $stats = shift;
    my $guess = shift;

    my $last_game_pos = scalar(@{$stats}) - 1;

    if ($last_game_pos >= 0 && scalar(@{$stats}[$last_game_pos]) == 1) {
        die "last game not finished yet";
    }
  
    push @{$stats}, [$guess];
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
  $num =~ s/0$|//g;

  if ($try >= length($num)) {
    $try = 0;
    $num *= 1.71828183;
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
mkdir (TMP_DIR) unless (-d TMP_DIR);
die "Invalid args" if (scalar(@ARGV) < 2);
my $opponent = $ARGV[0];
my $mode = $ARGV[1];
my $last_move = ($mode eq "turn" || $mode eq "finish") ? $ARGV[2] : undef;
my $stats = readStats($opponent);


$stats = [] unless $stats;

if ($mode eq "turn" || $mode eq "finish") {
    addLastStat($stats, $last_move);
}

if ($mode ne "finish") {
    my $guess = doGuess($stats);
    addCurrentStat($stats, $guess);
    print $guess, "\n";
}

writeStats($stats, $opponent);
