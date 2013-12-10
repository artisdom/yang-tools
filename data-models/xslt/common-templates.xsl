<?xml version="1.0"?>

<!-- Program name: common-templates.xsl

Copyright © 2013 by Ladislav Lhotka, CESNET <lhotka@cesnet.cz>

Common templates for YANG -> CLI translations.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="1.0">

  <xsl:variable name="NL" select="'&#xA;'"/>

  <xsl:template name="VALUE-NL">
    <xsl:value-of select="concat(., $NL)"/>
  </xsl:template>

  <xsl:template name="MOCT">
    <!-- Octet value corresponding to the length of a bitmask. -->
    <xsl:param name="bits"/>
    <xsl:choose>
      <xsl:when test="number($bits) &lt;= 0">0</xsl:when>
      <xsl:when test="number($bits) = 1">128</xsl:when>
      <xsl:when test="number($bits) = 2">192</xsl:when>
      <xsl:when test="number($bits) = 3">224</xsl:when>
      <xsl:when test="number($bits) = 4">240</xsl:when>
      <xsl:when test="number($bits) = 5">248</xsl:when>
      <xsl:when test="number($bits) = 6">252</xsl:when>
      <xsl:when test="number($bits) = 7">254</xsl:when>
      <xsl:otherwise>255</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="QMASK">
    <!-- Netmask in the dotted-quad format. -->
    <xsl:param name="mlen" select="32"/>
    <xsl:call-template name="MOCT">
      <xsl:with-param name="bits" select="$mlen"/>
    </xsl:call-template>
    <xsl:text>.</xsl:text>
    <xsl:call-template name="MOCT">
      <xsl:with-param name="bits" select="$mlen"/>
    </xsl:call-template>
    <xsl:text>.</xsl:text>
    <xsl:call-template name="MOCT">
      <xsl:with-param name="bits" select="$mlen - 16"/>
    </xsl:call-template>
    <xsl:text>.</xsl:text>
    <xsl:call-template name="MOCT">
      <xsl:with-param name="bits" select="$mlen - 24"/>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>