# Matlab/Octave script for displaying the occurrence matrix
# that is output by BZip2.Encoding (bzip2-encoding.adb) when
# the constant trace_frequency_matrix is True.
# See procedure Output_Frequency_Matrix for the actual output details.

z = dlmread('bzip2_freq_appnote_sorted_inv.csv', ';', 0, 0)';

bar3(z);

title('BZip2: frequencies of MTF & RLE2 symbols. Data: appnote.txt');
xlabel('String (strings are sorted by bumpiness)');
ylabel('Symbol');
zlabel('Occurrences');
