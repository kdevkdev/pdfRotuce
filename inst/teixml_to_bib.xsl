<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                version="1.0"   
                encoding="UTF-8"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="str">
  <xsl:output method="text"/>
<!--
Copyright 2011 TEI Consortium. All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such
damage.

Original from tei-c.org, with modifications by Kaspar Meili. 
-->
<xsl:template name="biblStruct2bibtex" match="biblStruct">
  <xsl:variable name="type">
    <xsl:choose>
      <xsl:when test="analytic/title and monogr/title[@level='j']">
        <xsl:text>article</xsl:text>
      </xsl:when>
      <xsl:when test="analytic/title and monogr/title[@level='m']">
        <xsl:text>incollection</xsl:text>
      </xsl:when>
      <xsl:when test="analytic/idno[@type='url']">
        <xsl:text>techreport</xsl:text>
      </xsl:when>
      <xsl:when test="series">
        <xsl:text>techreport</xsl:text>
      </xsl:when>
      <xsl:when test="not(analytic)">
        <xsl:text>book</xsl:text>
      </xsl:when>
      <xsl:when test="monogr/imprint/biblScope[@unit='volume']">
        <xsl:text>article</xsl:text>	    
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>incollection</xsl:text>	    
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:text>@</xsl:text>
  <xsl:value-of select="$type"/>
  <xsl:text>{</xsl:text>
  <xsl:choose>
    <xsl:when test="not($citekey='')">
      <xsl:value-of select="$citekey"></xsl:value-of>
    </xsl:when>
    <xsl:when test="@xml:id">
      <xsl:value-of select="@xml:id"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>__placeholdercitekey__</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>,&#10;</xsl:text>
  <xsl:variable name="all">
    <xsl:apply-templates mode="tobib"/>
    <xsl:if test="not($edition='')"><!-- add fields that depend on passed variables-->
      <xsl:text>@edition = {</xsl:text>
      <xsl:value-of select="$edition"/>
      <xsl:text>},&#10;</xsl:text>
    </xsl:if>
  </xsl:variable>
  <xsl:for-each select="str:tokenize($all,'@')">
  <xsl:if test="not(.='')">
  <xsl:text>	</xsl:text>
    <xsl:value-of select="."/>
    <xsl:if test="not(position()=last())">,</xsl:if>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
  </xsl:for-each>
  <xsl:text>}&#10;&#10;</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="publisher">
  <xsl:choose>
    <xsl:when test="ancestor::biblStruct/series or  ancestor::biblStruct/idno[@type='url']">
      <xsl:text>@institution={</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>@publisher={</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template mode="tobib" match="note">
  <xsl:text>@note={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template mode="tobib" match="meeting/address|imprint/pubPlace/address">
  <xsl:text>@address={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template mode="tobib" match="ptr[@target]">
  <xsl:text>@url={</xsl:text>
  <xsl:value-of select = "@target"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="idno[@type='url']">
  <xsl:text>@url={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="idno[@type='DOI']">
  <xsl:text>@doi={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="idno[@type='isbn']">
  <xsl:text>@isbn={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="pubPlace">
  <xsl:text>@address={</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="date">
  <xsl:text>@year={</xsl:text>
  <xsl:comment><!--Automatic processing not easily possible in xslt 1.0, as regex is lacking - do some additional  preprocessing to extract the year and pass as parameter--></xsl:comment>
  <xsl:choose>
    <xsl:when test="not($year='')">
      <xsl:value-of select="$year"/>      
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="title">
  <xsl:variable name="name">
    <xsl:choose>
      <xsl:when test="@level='a'">title</xsl:when>
      <xsl:when test="parent::monogr and not(ancestor::biblStruct/analytic)">title</xsl:when>
      <xsl:when test="@level='j' or parent::monogr/imprint/biblScope[@unit='volume']">journal</xsl:when>
      <xsl:when test="@level='m' or parent::monogr">booktitle</xsl:when>
      <xsl:when test="@level='s'">series</xsl:when>
      <xsl:when test="parent::series">type</xsl:when>
      <xsl:otherwise>title</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:text>@</xsl:text>
  <xsl:value-of select="$name"/>
  <xsl:text>={{</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}}</xsl:text>
</xsl:template>


<xsl:template mode="tobib" match="q">
  <xsl:text>``</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>''</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="series">
  <xsl:apply-templates mode="tobib"/>
</xsl:template>



<xsl:template mode="tobib" match="biblScope[@unit='page']">
  <xsl:text>@pages={</xsl:text>
  <xsl:choose>
    <xsl:when test="@from and @to and @from &lt;= @to">
      <xsl:value-of select="@from" />
      <xsl:text>--</xsl:text>
      <xsl:value-of select="@to" />
    </xsl:when>
    <xsl:when test="@from and @to and @from = @to">
      <xsl:value-of select="@from" />
    </xsl:when>
    <xsl:when test="@from and not(@to)">
      <xsl:value-of select="@from" />
    </xsl:when>
    <xsl:otherwise><xsl:value-of select = "."/></xsl:otherwise>
  </xsl:choose>
  <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template mode="tobib" match="biblScope[@unit='volume']">
  <xsl:text>@volume={</xsl:text>
    <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="biblScope[@unit='issue']">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="biblScope[@unit='number']">
  <xsl:value-of select="."/>
</xsl:template>


<xsl:template mode="tobib" match="biblScope[@unit='issue' or @unit='number']">
  <xsl:text>@issue={</xsl:text>
  <xsl:apply-templates mode="tobib"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template mode="tobib" match="edition">
  <xsl:text>@edition={</xsl:text>
    <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>




<xsl:template name="processFSnames">
    <xsl:for-each select="surname">
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:if test="surname and forename"><xsl:text>, </xsl:text></xsl:if>
    <xsl:for-each select="forename">
      <xsl:value-of select="."/>
    </xsl:for-each>
</xsl:template>

<xsl:template match="persName">
  <xsl:call-template name="processFSnames"/>
</xsl:template>

<xsl:template match="author">
  <xsl:call-template name="processFSnames"/>
</xsl:template>

<xsl:template match="editor">
  <xsl:call-template name="processFSnames"/>
</xsl:template>

<xsl:template mode="tobib" match="editor">
  <xsl:if test="not(preceding-sibling::editor)">
    <xsl:text>@editor={</xsl:text>
    <xsl:for-each select="../editor">
      <xsl:text>{</xsl:text>
      <xsl:choose>
        <xsl:when test="persName[surname and forename]"><xsl:apply-templates select="persName[surname and forename]"/></xsl:when>
        <xsl:when test="../editor[surname and forename]"><xsl:apply-templates select="../editor[surname and forename]"/></xsl:when>
        <xsl:otherwise><xsl:apply-templates /></xsl:otherwise>
      </xsl:choose>
      <xsl:text>}</xsl:text>
      <xsl:if test="following-sibling::editor"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template mode="tobib" match="author">
  <xsl:if test="not(preceding-sibling::author)">
    <xsl:text>@author={</xsl:text>
    <xsl:for-each select="../author">
      <xsl:text>{</xsl:text>
      <xsl:choose>
        <xsl:when test="persName[surname and forename]"><xsl:apply-templates select="persName[surname and forename]"/></xsl:when>
        <xsl:when test="../author[surname and forename]"><xsl:apply-templates select="../author[surname and forename]"/></xsl:when> 
        <xsl:otherwise><xsl:apply-templates /></xsl:otherwise>
      </xsl:choose>
      <xsl:text>}</xsl:text>
      <xsl:if test="following-sibling::author"> and </xsl:if>
    </xsl:for-each>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
