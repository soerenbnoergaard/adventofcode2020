with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;

with GNAT.Regpat;
use GNAT.Regpat;

procedure Main is
    type Password_Type is
        record
            Min: Natural;
            Max: Natural;
            Letter: Character;
            Password: Unbounded_String;
        end record;

    package Password_Lists is new Ada.Containers.Doubly_Linked_Lists(Password_Type);

    function Parse_Line(Line: Unbounded_String) return Password_Type is
        Re: constant Pattern_Matcher := Compile("(\d+)-(\d+)\s+(\w):\s+(\w+)\s*$");
        Matches: Match_Array(0 .. 4);
        Password: Password_Type;
        S: String := To_String(Line);
    begin
        Match(Re, S, Matches);
        Password.Min := Integer'Value(S(Matches(1).First .. Matches(1).Last));
        Password.Max := Integer'Value(S(Matches(2).First .. Matches(2).Last));
        Password.Letter := S(Matches(3).First);
        Password.Password := To_Unbounded_String(S(Matches(4).First .. Matches(4).Last));
        return Password;
    end;

    function Load_Database(Filename: String) return Password_Lists.List is
        Database: Password_Lists.List;
        Line: Unbounded_String;
        Fp: File_Type;
    begin
        Open(Fp, In_File, Filename);
        loop
            exit when End_Of_File(Fp);
            Line := Get_Line(Fp);
            Database.Append(Parse_Line(Line));
        end loop;

        Close(Fp);
        return Database;
    end;

    function Count_Letters(Input: Unbounded_String; Letter: Character) return Natural is
        N: Natural := 0;
    begin
        for I in 1 .. Length(Input) loop
            if Element(Input, I) = Letter then
                N := N + 1;
            end if;
        end loop;
        return N;
    end;

    function Password_Ok_1(Password: Password_Type) return Boolean is
        N: Natural := Count_Letters(Password.Password, Password.Letter);
        Ok: Boolean;
    begin

        if N < Password.Min then
            Ok := False;
        elsif N <= Password.Max then
            Ok := True;
        else
            Ok := False;
        end if;
        return Ok;
    end;

    function Password_Ok_2(Password: Password_Type) return Boolean is
        Ok: Boolean := False;
        Num_Found: Natural := 0;
    begin
        if Element(Password.Password, Password.Min) = Password.Letter then
            Num_Found := Num_Found + 1;
        end if;

        if Element(Password.Password, Password.Max) = Password.Letter then
            Num_Found := Num_Found + 1;
        end if;

        if Num_Found = 1 then
            return True;
        else
            return False;
        end if;
    end;


    Database: Password_Lists.List;
    Num_Passwords: Natural := 0;
    Num_Valid_Passwords_1: Natural := 0;
    Num_Valid_Passwords_2: Natural := 0;

begin
    Put_Line("Advent of Code - Day 2");
    Database := Load_Database("puzzle_input.txt");

    for Password of Database loop
        Num_Passwords := Num_Passwords + 1;

        if Password_Ok_1(Password) then
            Num_Valid_Passwords_1 := Num_Valid_Passwords_1 + 1;
        end if;

        if Password_Ok_2(Password) then
            Num_Valid_Passwords_2 := Num_Valid_Passwords_2 + 1;
        end if;
    end loop;

    Put("Total passwords:     ");
    Put(Num_Passwords);
    New_Line;

    Put("Valid passwords (1): ");
    Put(Num_Valid_Passwords_1);
    New_Line;

    Put("Valid passwords (2): ");
    Put(Num_Valid_Passwords_2);
    New_Line;

end Main;

