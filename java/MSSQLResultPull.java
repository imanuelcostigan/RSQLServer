package info.urbanek.Rpackage.RJDBC;

import java.sql.ResultSet;

public class JDBCResultPull {
    /** column type: string */
    public static final int CT_STRING  = 0;
    /** column type: numeric (retrieved as doubles) */
    public static final int CT_NUMERIC = 1;
    /** NA double value */
    public static final double NA_double = Double.longBitsToDouble(0x7ff00000000007a2L);

    /** active result set */
    ResultSet rs;
    /** column types */
    int cTypes[];
    /** pulled arrays */
    Object data[];
    /** capacity of the arrays */
    int capacity;
    /** number of loaded rows */
    int count;
    /** number of columns */
    int cols;

    /** create a JDBCResultPull from teh current set with the
     * specified column types. The column type definition must match
     * the result set, no checks are performed.
     * @param rs active result set
     * @param cTypes column types (see <code>CT_xx</code> constants)
     */
    public JDBCResultPull(ResultSet rs, int cTypes[]) {
	this.rs = rs;
	this.cTypes = cTypes;
	cols = (cTypes == null) ? 0 : cTypes.length;
	data = new Object[cols];
	capacity = -1;
	count = 0;
    }

    /** retrieve the number of columns */
    public int columns() { return cols; }
    
    /** get the number of loaded rows */
    public int count() { return count; }

    /** allocate arrays for the given capacity. Normally this method
     * is not called directly since @link{fetch()} automatically
     * allocates necessary space, but it can be used to reduce the
     * array sizes when idle (e.g., by setting the capacity to 0).
     * @param atMost maximum capacity of the buffers
     */
    public void setCapacity(int atMost) {
	if (capacity != atMost) {
	    for (int i = 0; i < cols; i++)
		data[i] = (cTypes[i] == CT_NUMERIC) ? (Object)new double[atMost] : (Object)new String[atMost];
	    capacity = atMost;
	}
    }

    /** fetch records from the result set into column arrays. It
     * replaces any existing data in the buffers.
     * @param atMost the maximum number of rows to be retrieved
     * @param fetchSize fetch size hint to be sent to the driver. Note
     * that some databases don't support fetch sizes larger than
     * 32767. If less than 1 the fetch size is not changed.
     * @return number of rows retrieved
     */
    public int fetch(int atMost, int fetchSize) throws java.sql.SQLException {
	setCapacity(atMost);
	if (fetchSize > 0) {
	    try { // run in a try since it's a hint, but some bad drivers fail on it anyway (see #11)
		rs.setFetchSize(fetchSize);
	    } catch (java.sql.SQLException e) { } // we can't use SQLFeatureNotSupportedException because that's 1.6+ only
	}
	count = 0;
	while (rs.next()) {
	    for (int i = 0; i < cols; i++)
		if (cTypes[i] == CT_NUMERIC) {
		    double val = rs.getDouble(i + 1);
		    if (rs.wasNull()) val = NA_double;
		    ((double[])data[i])[count] = val; 
		} else
		    ((String[])data[i])[count] = rs.getString(i + 1); 
	    count++;
	    if (count >= capacity)
		return count;
	}
	return count;
    }
    
    /** retrieve column data
     *  @param column 1-based index of the column
     *  @return column object or <code>null</code> if non-existent */
    public Object getColumnData(int column) {
	return (column > 0 && column <= cols) ? data[column - 1] : null;
    }

    /** retrieve string column data truncated to count - performs NO
     *  checking and can raise exceptions
     *  @param column 1-based index of the column
     *  @return column object or <code>null</code> if non-existent */
    public String[] getStrings(int column) {
	String[] a = (String[]) data[column - 1];
	if (count == a.length) return a;
	String[] b = new String[count];
	if (count > 0) System.arraycopy(a, 0, b, 0, count);
	return b;
    }

    /** retrieve numeric column data truncated to count - performs NO
     *  checking and can raise exceptions
     *  @param column 1-based index of the column
     *  @return column object or <code>null</code> if non-existent */
    public double[] getDoubles(int column) {
	double[] a = (double[]) data[column - 1];
	if (count == a.length) return a;
	double[] b = new double[count];
	if (count > 0) System.arraycopy(a, 0, b, 0, count);
	return b;
    }
}