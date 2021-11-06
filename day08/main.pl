use strict;
use warnings;
use Data::Dumper;

# my $filename = "test_input.txt";
my $filename = "puzzle_input.txt";
open(my $fh, "<", $filename) or die "Could not open file";

# Parse program

my @program = ();
for my $line (<$fh>) {
    $line =~ m/^(\w+)\s+(.*?)$/;
    push @program, [$1, int($2)];
}

# Run program
run_program(\@program);

# Change instructions in the program one at a time to see if it solves the
# issue (swapping nop and jmp instructions)

my $pc = 0;
foreach (@program) {
    my ($inst, $arg) = @{$_};
    print("Changing line $pc\n");

    # Deep copy program
    my @patched_program = map { [@$_] } @program;

    if ($inst eq "nop") {
        $patched_program[$pc][0] = "jmp";
    }
    elsif ($inst eq "jmp") {
        $patched_program[$pc][0] = "nop";
    }

    if (run_program(\@patched_program) == 0) {
        last;
    }

    $pc += 1;
}

# Return 0 if succesful
# Return 1 if infinite loop occurs
sub run_program {
    my @program = @{$_[0]};
    my $program_length = scalar @program;
    my @visited = (0) x $program_length;
    my $pc = 0;
    my $acc = 0;

    while ($pc < $program_length-1) {
        my ($inst, $arg) = @{$program[$pc]};
        # print "$inst $arg $pc $acc\n";

        if ($visited[$pc] > 0) {
            print("Infinite loop occured with acc = $acc\n");
            return 1;
        }
        else {
            $visited[$pc] += 1;
        }

        if ($inst eq "nop") {
            # Do nothing
            $pc += 1;
        }
        elsif ($inst eq "acc") {
            $acc += $arg;
            $pc += 1;
        }
        elsif ($inst eq "jmp") {
            $pc += $arg;
        }
        else {
            die "Unknown instruction: $inst\n";
        }
    }
    print "Program terminated with acc = $acc\n";
    return 0;
}
