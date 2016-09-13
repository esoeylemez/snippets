/*-
 * Copyright (c) 2016, Drew Mason-Laurence
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTOR
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/*
 * A function to find the longest palendrome in a string.
 */

char*
find_long_palindrome(char* str_in, int str_len)
{
    /* Declare variables for the function */
    int i, j, k;
    int pal_len, pal_start;
    int l_pal_len = 0;
    int l_pal_start = 0;
    char* str_out;

    /* Iterate through all the characters in the string */
    for (i = 1; (i + 1) < str_len; i++) {
        /* Detection for odd palindrome */
        if (str_in[i-1] == str_in[i+1]) {
            pal_start = i-1;
            pal_len = 3;
            /*
             * Iterate around the palindrome to see
             * if it is bigger.
             */
            for (j = 2; (i - j) >= 0 && (i + j) < str_len; j++) {
                /* If it is bigger, update temp vars */
                if (str_in[i-j] == str_in[i+j]) {
                    pal_start = i - j;
                    pal_len = (j * 2) + 1;
                } else {
                    break;
                }
            }
            /*
             * Check to see if this palindrome is longer than
             * The previous longest palindrome, if it is
             * update the vars.
             */
            if (pal_len > l_pal_len) {
                l_pal_len = pal_len;
                l_pal_start = pal_start;
            }
        /* Code to detect even palindromes */
        }
        if (str_in[i] == str_in[i+1]) {
            pal_start = i;
            pal_len = 2;
            /*
             * Iterate around the palindrome to see
             * if it is bigger.
             */
            for (j = 1; (i - j) >= 0 && (i + j + 1) < str_len; j++)
            {
                /* If it is bigger, update temp vars */
                if (str_in[i-j] == str_in[i+j+1]) {
                    pal_start = i - j;
                    pal_len = (j * 2) + 2;
                } else {
                    break;
                }
            }
            if (pal_len > l_pal_len) {
                l_pal_len = pal_len;
                l_pal_start = pal_start;
            }
        }
    }
    /* Allocate memory on the heap for the palindrome */
    str_out = (char*)malloc(sizeof(char) * l_pal_len + 1);
    /*
     * Copy the palindrome from the main strin, str_in,
     * into the return string, str_out.
     */
    for (k = 0; k < l_pal_len; k++) {
        str_out[k] = str_in[l_pal_start+k];
    }
    str_out[l_pal_len] = 0;
    return str_out;

}

int
main(int argc, char* argv[])
{
    /* Declare a clock, t, to tell the time */
    clock_t t;

    /* Open and read to str_in the file given in the argument */
    FILE *f = fopen(argv[1], "rb");
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* str_in = malloc(fsize + 1);
    fread(str_in, fsize, 1, f);
    fclose(f);

    str_in[fsize] = 0;

    /* Find the total length of the string. */
    int str_len = strlen(str_in);


    /* Assign the current clock to t */
    t = clock();
    /* Find the longest palindrome */
    char* l_pal = find_long_palindrome(str_in, str_len);
    /* Set t to the value of the current clock - the earlier clock */
    t = clock() - t;

    /* Print the result to the terminal */
    printf("Longest Palindrome = |%s|, time = %f\n",
        l_pal, ((float)t)/CLOCKS_PER_SEC);
    printf("String Length: %d\n", str_len);
    printf("Palindrome Length: %d\n", (int)strlen(l_pal));
    return 0;
}
