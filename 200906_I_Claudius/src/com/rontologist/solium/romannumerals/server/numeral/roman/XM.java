package com.rontologist.solium.romannumerals.server.numeral.roman;

public class XM extends AbstractRomanNumeral {
    private static final XM INSTANCE = new XM();
    private static final int VALUE = 990;

    private XM() { }

    public static XM getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}