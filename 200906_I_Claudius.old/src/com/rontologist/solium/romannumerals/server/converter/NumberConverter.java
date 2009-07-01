package com.rontologist.solium.romannumerals.server.converter;

import com.rontologist.solium.romannumerals.server.number.Number;

public interface NumberConverter<T extends Number, U extends Number> {
    public U convert(T number);
}