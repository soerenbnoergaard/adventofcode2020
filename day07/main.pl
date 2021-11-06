use strict;
use warnings;
use Data::Dumper;

# my %rules = get_rules("test_input.txt");
# my %rules = get_rules("test_input2.txt");
my %rules = get_rules("puzzle_input.txt");

my %shiny_gold_count = get_shiny_gold_count(\%rules);

# print Dumper(\%rules);
# print Dumper(\%shiny_gold_count);

my $task1_count = 0;
while (my ($color, $count) = each (%shiny_gold_count)) {
    if ($count > 0) {
        $task1_count += 1;
    }
}
print "TASK 1: Number of bags with at least one shiny gold: $task1_count\n";

print "TASK 2: Number of bags inside one shiny gold: " . get_num_bags_in_shiny_gold() . "\n";

sub get_num_bags_in_shiny_gold {
    return count_all_bags(\%{$rules{"shiny gold"}}) - 1;
}

sub get_shiny_gold_count {
    my %rules = %{$_[0]};
    my %shiny_gold_count;

    foreach my $outer_color (keys %rules) {
        $shiny_gold_count{$outer_color} = count_shiny_gold(\%{$rules{$outer_color}});
    }
    return %shiny_gold_count;
}

sub get_rules {
    my $filename = $_[0];

    open(my $fh, "<", $filename) or die("Could not open");
    my @lines = <$fh>;

    # Build a general rules table
    my %rules = ();
    foreach my $line (@lines) {
        my ($outer_color, $content_string) = $line =~ m/^(.*?) bags contain (.*?)$/;
        my @content = split(",", $content_string);

        # if ($outer_color eq "shiny gold") {
        #     next;
        # }

        # Add default count for shiny gold
        $rules{$outer_color}{"shiny gold"} = 0;

        foreach my $element (@content) {
            my ($count, $inner_color) = $element =~ m/(\d+) ([\w\s]+) bags?/ or next;

            $rules{$outer_color}{$inner_color} = $count;
        }
    }
    return %rules;
}

sub count_all_bags {
    my %hash = %{$_[0]};
    my $count = 1;

    # foreach my $color (keys %hash) {
    while (my ($color, $value) = each (%hash)) {
        if ($value > 0) {
            $count += $value * count_all_bags(\%{$rules{$color}});
        }
    }
    return $count;
}

sub count_shiny_gold {
    my %hash = %{$_[0]};
    my $count = 0;

    # foreach my $color (keys %hash) {
    while (my ($color, $value) = each (%hash)) {
        if ($color eq "shiny gold") {
            $count += $value;
        }
        else {
            $count += $value * count_shiny_gold(\%{$rules{$color}});
        }
    }
    return $count;
}

