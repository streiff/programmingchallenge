package com.rontologist.solium.romannumerals.server.numeral.roman;

public class IC extends AbstractRomanNumeral {
    private static final IC INSTANCE = new IC();
    private static final int VALUE = 99;

    private IC() { }

    public static IC getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}