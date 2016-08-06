package com.github.RSQLServer;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Types;
import java.sql.Date;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;

// Based on:
// https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/java/JDBCResultPull.java

public class MSSQLResultPull {
  public static final boolean RP_DEBUG = false;
  /** column type: string */
  public static final int CT_STRING  = 0;
  /** column type: numeric (retrieved as doubles) */
  public static final int CT_NUMERIC = 1;
  /** column type: integer */
  public static final int CT_INT = 2;
  /** column type: Date */
  public static final int CT_DATE = 3;
  /** column type: Timestamp */
  public static final int CT_TIME = 4;
  /** column type: bit/boolean */
  public static final int CT_BOOLEAN = 5;
  /** NA double value */
  public static final double NA_double = Double.longBitsToDouble(0x7ff00000000007a2L);
  // NA int value (also used for booleans).
  //rJava::.jnew("java.lang.Integer", NA_integer_)
  // [1] "Java-Object{-2147483648}"
  public static final int NA_int = -2147483648;

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

  /** create a MSSQLResultPull from teh current set with the
  * specified column types. The column type definition must match
  * the result set, no checks are performed.
  * @param rs active result set
  * @param cTypes column types (see <code>CT_xx</code> constants)
  */
  public MSSQLResultPull(ResultSet rs) throws java.sql.SQLException {
    this.rs = rs;
    cTypes = mapColumns();
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
      for (int i = 0; i < cols; i++) {
        switch (cTypes[i]) {
          case CT_NUMERIC:
            data[i] = (Object)new double[atMost];
            break;
          case CT_INT:
            data[i] = (Object)new int[atMost];
            break;
          case CT_DATE:
            data[i] = (Object)new String[atMost];
            break;
          case CT_TIME:
            data[i] = (Object)new String[atMost];
            break;
          case CT_BOOLEAN:
            data[i] = (Object)new String[atMost];
            break;
          default:
            data[i] = (Object)new String[atMost];
        }
      }
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
  // formatters that may be useful. Declare here to avoid creating over and over
  // again in loop
  SimpleDateFormat tsFmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSSSSSSS");
  SimpleDateFormat dtFmt = new SimpleDateFormat("yyyy-MM-dd");

  // Fetch into `data`
  while (rs.next()) {
    if (RP_DEBUG) System.out.println("Row " + (count + 1));
    for (int i = 0; i < cols; i++) {
      if (RP_DEBUG) System.out.println("  Column " + (i + 1) + ". Column type: " + cTypes[i]);
      switch(cTypes[i]) {
        case CT_NUMERIC:
          double val = rs.getDouble(i + 1);
          if (rs.wasNull()) val = NA_double;
          if (RP_DEBUG) System.out.println("  Double type: " + val);
          ((double[])data[i])[count] = val;
          break;
        case CT_INT:
          int valint = rs.getInt(i + 1);
          if (rs.wasNull()) valint = NA_int;
          ((int[])data[i])[count] = valint;
          if (RP_DEBUG) System.out.println("  Int type: " + valint);
          break;
        case CT_TIME:
          Timestamp valdtime = rs.getTimestamp(i + 1);
          if (RP_DEBUG) System.out.println("  wasNull: " + rs.wasNull());
          ((String[])data[i])[count] = rs.wasNull() ? null : tsFmt.format(valdtime);
          if (RP_DEBUG) System.out.println("  Timestamp: " + valdtime);
          break;
        case CT_DATE:
          Date valdt = rs.getDate(i + 1);
          ((String[])data[i])[count] = rs.wasNull() ? null : dtFmt.format(valdt);
          if (RP_DEBUG) System.out.println("  Date/String type: " + valdt);
          break;
        case CT_BOOLEAN:
          String valbit = String.valueOf(rs.getBoolean(i + 1));
          if (rs.wasNull()) valbit = null;
          ((String[])data[i])[count] = valbit;
          if (RP_DEBUG) System.out.println("  Boolean/String type: " + valbit);
          break;
        default:
          String valstr = rs.getString(i + 1);
          ((String[])data[i])[count] = valstr;
          if (RP_DEBUG) System.out.println("  Str type: " + valstr);
      }
    }
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

public int[] getInts(int column) {
  int[] a = (int[]) data[column - 1];
  if (count == a.length) return a;
  int[] b = new int[count];
  if (count > 0) System.arraycopy(a, 0, b, 0, count);
  return b;
}

//  public Date[] getDates(int column) {
//      Date[] a = (Date[]) data[column - 1];
//      if (count == a.length) return a;
//      Date[] b = new Date[count];
//      if (count > 0) System.arraycopy(a, 0, b, 0, count);
//      return b;
//  }

// public Timestamp[] getTimestamps(int column) {
//      Timestamp[] a = (Timestamp[]) data[column - 1];
//      if (count == a.length) return a;
//      Timestamp[] b = new Timestamp[count];
//      if (count > 0) System.arraycopy(a, 0, b, 0, count);
//      return b;
//  }

public int[] mapColumns() throws java.sql.SQLException {
  ResultSetMetaData md = rs.getMetaData();
  int n = md.getColumnCount();
  int[] cts = new int[n];
  for (int i = 0; i < n; i++) {
    int ct = md.getColumnType(i + 1);
    if (ct == Types.BIGINT || ct == Types.NUMERIC || ct == Types.DECIMAL ||
    ct == Types.FLOAT || ct == Types.REAL || ct == Types.DOUBLE) {
      cts[i] = CT_NUMERIC;
    } else if (ct == Types.TINYINT || ct == Types.SMALLINT || ct == Types.INTEGER) {
      cts[i] = CT_INT;
    } else if (ct == Types.DATE) {
      cts[i] = CT_DATE;
    } else if (ct == Types.TIMESTAMP) {
      cts[i] = CT_TIME;
    } else if (ct == Types.BIT || ct == 16) {
      // BOOLEAN = 16 type introduced sometime after 1.3.
      cts[i] = CT_BOOLEAN;
    } else {
      cts[i] = CT_STRING;
    }
  }
  return cts;
}

public int[] columnTypes() throws java.sql.SQLException {
  ResultSetMetaData md = rs.getMetaData();
  int n = md.getColumnCount();
  int[] cts = new int[n];
  for (int i = 0; i < n; i++) {
    cts[i] = md.getColumnType(i + 1);
  }
  return cts;
}

public String[] columnTypeNames() throws java.sql.SQLException {
  ResultSetMetaData md = rs.getMetaData();
  int n = md.getColumnCount();
  String[] cts = new String[n];
  for (int i = 0; i < n; i++) {
    cts[i] = md.getColumnTypeName(i + 1);
  }
  return cts;
}

public String[] columnNames() throws java.sql.SQLException {
  ResultSetMetaData md = rs.getMetaData();
  int n = md.getColumnCount();
  String[] cnames = new String[n];
  for (int i = 0; i < n; i++) {
    cnames[i] = md.getColumnName(i + 1);
  }
  return cnames;
}

// public String[] getClassOfData() throws java.sql.SQLException {
//     String[] out = new String[cols];
//     for (int i = 0; i < cols; i++) {
//         out[i] = data[i].getClass().toString();
//     }
//     return out;
// }
}
