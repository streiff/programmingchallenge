package com.rontologist.solium.romannumerals.server.converter;

import com.rontologist.solium.romannumerals.server.number.Number;
import com.rontologist.solium.romannumerals.server.number.UnaryNumber;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;

public class NumberConverterFactory {
    private static final NumberConverterFactory INSTANCE = new NumberConverterFactory();

    private NumberConverterFactory() {}

    public static NumberConverterFactory getInstance() {
        return INSTANCE;
    }


    @SuppressWarnings({"unchecked"})
    public <T extends Number, U extends Number>
    NumberConverter<T, U> getConverter(Class<T> src, Class<U> dest) {
        if (src.equals(UnaryNumber.class) && dest.equals(RomanNumber.class)) {
            return (NumberConverter<T, U>) new UnaryToRomanNumberConverter();
        } else {
            throw new RuntimeException("Don't know how to convert between " +
                src.getName() + " and " + dest.getName());
        }
    }

}