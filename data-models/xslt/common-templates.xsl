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

<stylesheet xmlns="http://www.w3.org/1999/XSL/Transform"
	    version="1.0">

  <!-- Amount of indentation added for each BIRD hierarchy level. -->
  <param name="indent-step" select="2"/>
  <!-- Maximum line length -->
  <param name="line-length" select="70"/>

  <variable name="NL" select="'&#xA;'"/>

  <template name="VALUE-NL">
    <value-of select="concat(., $NL)"/>
  </template>

  <template name="MOCT">
    <!-- Octet value corresponding to the length of a bitmask. -->
    <param name="bits"/>
    <choose>
      <when test="number($bits) &lt;= 0">0</when>
      <when test="number($bits) = 1">128</when>
      <when test="number($bits) = 2">192</when>
      <when test="number($bits) = 3">224</when>
      <when test="number($bits) = 4">240</when>
      <when test="number($bits) = 5">248</when>
      <when test="number($bits) = 6">252</when>
      <when test="number($bits) = 7">254</when>
      <otherwise>255</otherwise>
    </choose>
  </template>

  <template name="QMASK">
    <!-- Netmask in the dotted-quad format. -->
    <param name="mlen" select="32"/>
    <call-template name="MOCT">
      <with-param name="bits" select="$mlen"/>
    </call-template>
    <text>.</text>
    <call-template name="MOCT">
      <with-param name="bits" select="$mlen"/>
    </call-template>
    <text>.</text>
    <call-template name="MOCT">
      <with-param name="bits" select="$mlen - 16"/>
    </call-template>
    <text>.</text>
    <call-template name="MOCT">
      <with-param name="bits" select="$mlen - 24"/>
    </call-template>
  </template>

  <template name="repeat-string">
    <param name="count"/>
    <param name="string"/>
    <choose>
      <when test="not($count) or not($string)"/>
      <when test="$count = 1">
	<value-of select="$string"/>
      </when>
      <otherwise>
	<if test="$count mod 2">
	  <value-of select="$string"/>
	</if>
	<call-template name="repeat-string">
	  <with-param name="count" select="floor($count div 2)"/>
	  <with-param name="string" select="concat($string,$string)"/>
	</call-template> 
      </otherwise>
    </choose>
  </template>

  <template name="indent">
    <!-- Insert number of spaces corresponding to indentation at `lev` -->
    <param name="level" select="1"/>
    <call-template name="repeat-string">
      <with-param name="count" select="$level * $indent-step"/>
      <with-param name="string" select="' '"/>
    </call-template>
  </template>

  <template name="fill-text">
    <param name="text"/>
    <param name="length"/>
    <param name="remains" select="$length"/>
    <param name="prefix"/>
    <param name="wdelim" select="' '"/>
    <param name="break" select="'&#xA;'"/>
    <param name="at-start" select="false()"/>
    <if test="string-length($text) &gt; 0">
      <variable name="next-word">
	<choose>
	  <when test="contains($text, $wdelim)">
	    <value-of select="substring-before($text, $wdelim)"/>
	  </when>
	  <otherwise>
	    <value-of select="$text"/>
	  </otherwise>
	</choose>
      </variable>
      <variable name="rest">
	<choose>
	  <when test="contains($text, $wdelim)">
	    <value-of select="substring-after($text, $wdelim)"/>
	  </when>
	  <otherwise>
	    <text></text>
	  </otherwise>
	</choose>
      </variable>
      <variable
	  name="left"
	  select="$remains - string-length(concat($wdelim,$next-word))"/>
      <choose>
	<when test="$at-start">
	  <value-of select="$next-word"/>
	  <call-template name="fill-text">
	    <with-param name="text" select="$rest"/>
	    <with-param name="length" select="$length"/>
	    <with-param name="remains" select="$left + 1"/>
	    <with-param name="prefix" select="$prefix"/>
	    <with-param name="wdelim" select="$wdelim"/>
	    <with-param name="break" select="$break"/>
	  </call-template>
	</when>
	<when test="$left &lt; string-length($break)">
	  <value-of select="concat($break,$prefix)"/>
	  <call-template name="fill-text">
	    <with-param name="text" select="$text"/>
	    <with-param name="length" select="$length"/>
	    <with-param name="remains" select="$length"/>
	    <with-param name="prefix" select="$prefix"/>
	    <with-param name="wdelim" select="$wdelim"/>
	    <with-param name="break" select="$break"/>
	    <with-param name="at-start" select="true()"/>
	  </call-template>
	</when>
	<otherwise>
	  <value-of select="concat($wdelim,$next-word)"/>
	  <call-template name="fill-text">
	    <with-param name="text" select="$rest"/>
	    <with-param name="length" select="$length"/>
	    <with-param name="remains" select="$left"/>
	    <with-param name="prefix" select="$prefix"/>
	    <with-param name="wdelim" select="$wdelim"/>
	    <with-param name="break" select="$break"/>
	  </call-template>
	</otherwise>
      </choose>
    </if>
  </template>

</stylesheet>