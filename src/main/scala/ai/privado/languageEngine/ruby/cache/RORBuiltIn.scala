/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.ruby.cache

val RORBuiltInPrefix = "__builtin_ror"

object RORBuiltIn {

  val models = Set(
    "accepts_nested_attributes_for",
    "after_commit",
    "after_find",
    "after_initialize",
    "after_save",
    "attr_accessible",
    "attr_protected",
    "before_commit",
    "before_destroy",
    "before_save",
    "belong_to",
    "belongs_to",
    "concerns",
    "counter_cache",
    "default_scope",
    "dependent",
    "enum",
    "has_and_belongs_to_many",
    "has_many",
    "has_one",
    "has_paper_trail",
    "has_secure_password",
    "pluck",
    "polymorphic",
    "scope",
    "serialize",
    "touch",
    "transaction",
    "validates",
    "validates_format_of",
    "validates_length_of",
    "validates_numericality_of",
    "validates_uniqueness_of",
    "validates_presence_of"
  )

  val controllers = Set("before_action", "respond_to", "respond_with", "render", "redirect_to")

  def generateName(callName: String) = s"$RORBuiltInPrefix.$callName"

}
