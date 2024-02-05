(* fsharplint:disable TypeNames RecordFieldNames UnionCasesNames ModuleNames MemberNames ParameterNames PublicValuesNames NonPublicValuesNames
FL0038, FL0039, FL0041, FL0042, FL0045, FL0046, FL0049, FL0050
See
https://fsprojects.github.io/FSharpLint/how-tos/rule-configuration.html#ruleList
*)

module Trade_Breakdown

// Environment
open System
// StringBuilder
open System.Text

(* To install, run
dotnet add <.fsproj> package MathNet.Numerics.FSharp
(end)
*)
open MathNet.Numerics.Statistics

open Tag_Types
open Tag
open Data
open Built_In_Tag
open Transform_Types

(* Data types. *)

type private Data_And_Extracted_Value = {
    data : Data
    value : float
}

type private Group = {
    id : int
    parent_id : int option
    name : string
    total_value : float
    total_value_portion_of_all_groups_for_this_layer : float
    count : int
    count_portion_of_all_groups_for_this_layer : float
    average_value : float
    average_value_portion_of_all_groups_for_this_layer : float
    median_value : float
    data_list : Data_And_Extracted_Value list
}

(* Helper functions. *)

let mutable private next_group_id_ = 0

let private get_next_group_id () =
    do next_group_id_ <- next_group_id_ + 1
    next_group_id_

let private trade_breakdown_results_to_string (value_to_extract : string) (groups_1 : Group list) : string =
    let rec helper (acc_1 : StringBuilder) (indent_level : int) (group_1 : Group) : StringBuilder =
        let indent = String.replicate indent_level " "
        let group_name = $"{indent}{group_1.name}"
(* We cannot find a way to specify the thousands separator for the integer part of a float using F# string formatting.
See:
https://stackoverflow.com/a/4449135
https://stackoverflow.com/a/40830675
(end)
*)
        let number_format = "{0:#,##0.00}"
        let sum = String.Format (number_format, group_1.total_value)
        let sum_percentage = $"%.2f{group_1.total_value_portion_of_all_groups_for_this_layer * 100.0}%%"
        let count = group_1.count
        let count_percentage = $"%.2f{group_1.count_portion_of_all_groups_for_this_layer * 100.0}%%"
        let average = String.Format (number_format, group_1.average_value)
        let average_percentage = $"%.2f{group_1.average_value_portion_of_all_groups_for_this_layer * 100.0}%%"
        let median = String.Format (number_format, group_1.median_value)
        let acc_2 = acc_1.AppendFormat("{0,-30}{1,20}{2,10}{3,10}{4,10}{5,20}{6,10}{7,20}{8}", group_name, sum, sum_percentage, count, count_percentage, average, average_percentage, median, Environment.NewLine)

        match groups_1 |> List.filter (fun group_2 ->
            match group_2.parent_id with
            | Some parent_id -> parent_id = group_1.id
            | None -> false
        ) with
        | [] -> acc_2
        | groups_3 -> (acc_2, groups_3) ||> List.fold (fun acc_3 group_4  -> helper acc_3 (indent_level + 1) group_4)
    let acc_4 = StringBuilder ($"Metric: %s{value_to_extract}%s{Environment.NewLine}%s{Environment.NewLine}")
    let acc_5 = acc_4.AppendFormat("{0,-30}{1,20}{2,10}{3,10}{4,10}{5,20}{6,10}{7,20}{8}", "Group name", "Sum", "Percent", "Count", "Percent", "Average", "Percent", "Median", Environment.NewLine)
    let acc_6 = (acc_5, groups_1 |> List.filter (fun group -> group.parent_id.IsNone)) ||> List.fold (fun acc_7 group_5 ->
        helper (StringBuilder ()) 0 group_5 |> acc_7.Append
    )
    acc_6.ToString ()

(* Main functions. *)

let private trade_breakdown_3 (parent_id : int option) (layer : Tag_Id) (data_list_1 : Data_And_Extracted_Value list) : Group list =
    let values_1 = data_list_1 |> List.map _.value
    let total_for_all_groups_for_this_layer = values_1 |> List.sum
    let count_for_all_groups_for_this_layer = values_1 |> List.length
    let average_for_all_groups_for_this_layer = values_1 |> List.average
    data_list_1
(* For each Data, extract the value for the tag with Tag_Id *layer*. Group the Data by these values. Name the group according to the layer tag value. *)
        |> List.groupBy (fun data -> extract_tag_value_from_data layer data.data |> tag_value_to_string)
(* Loop through the groups. *)
        |> List.map (fun (group_name, data_list_2) ->
(* Combine the values for this group. *)
            let values_2 = data_list_2 |> List.map _.value
            let total_for_group = List.sum values_2
            let count_for_group = List.length values_2
            let average_for_group = List.average values_2
            let median_for_group = Statistics.Median values_2
            {
                id = get_next_group_id ()
                parent_id = parent_id
                name = group_name
                total_value = total_for_group
                total_value_portion_of_all_groups_for_this_layer = total_for_group / total_for_all_groups_for_this_layer
                count = count_for_group
                count_portion_of_all_groups_for_this_layer = (float count_for_group) / (float count_for_all_groups_for_this_layer)
                average_value = average_for_group
                average_value_portion_of_all_groups_for_this_layer = average_for_group / average_for_all_groups_for_this_layer
                median_value = median_for_group
(* Retain the Data list for this group. *)
                data_list = data_list_2
            }
        )

let rec private trade_breakdown_2 (parent_id : int option) (layers_1 : Tag_Id list) (data_list_1 : Data_And_Extracted_Value list) =
    match layers_1 with
    | [] -> []
    | layer :: layers_2 ->
(* Break the Data list into groups for this layer. *)
        let groups = trade_breakdown_3 parent_id layer data_list_1
(* Retain the groups, but also recurse with the remaining layers for each group. Combine the groups with the recursion results. *)
        groups @ (groups |> List.collect (fun group ->
(* This group becomes the parent of the groups returned by the recursion. *)
            trade_breakdown_2 (Some group.id) layers_2 group.data_list))

let private trade_breakdown_1 (tags : Built_In_Tags) (layers_1 : string) (value_to_extract_1 : string) (data_list_1 : Data list) : Data list =
(* Separate the layers and convert them to Tag_Ids. *)
    let layers_2 =
        layers_1.Split ','
            |> Array.toList
            |> List.map (fun tag_name -> tag_name_to_tag_id (Tag_Name tag_name) tags)
(* Convert the tag name of the value to extract to a Tag_Id. *)
    let value_to_extract_2 = tag_name_to_tag_id (Tag_Name value_to_extract_1) tags

    let groups =
        data_list_1
(* For each Data, extract the value. *)
            |> List.map (fun data ->
                {
                    value = extract_float_from_data value_to_extract_2 data
                    data = data
                }
            )
(* Convert the Data to groups for each layer. *)
            |> trade_breakdown_2 None layers_2
(* Sort the groups by parent group ID. *)
            |> List.sortBy (fun group ->
                match group.parent_id with
                | None -> 0
                | Some parent -> parent
            )

(* In other post system evaluation transforms we call add_tags_to_output_data_for_post_system_evaluation_transform. We do not do that here because add_tags_to_output_data_for_post_system_evaluation_transform requires a source Data from which to copy the series tags. We do not have a Data whose series tags make sense in that context. For example, this transform typically does not filter on system permutation ID (which is a series tag), because it breaks system evaluation results down by system permutation ID. So if we obtain the source Data with data_list_1 |> List.head, it might contain a random system permutation ID that we do not want copied to the output.
See also the comments in Evaluate_System_Helpers.post_system_evaluation_transform_results_to_string.
*)
(* Convert the groups to a string and add it to the result Data. *)
    add_tag_ids_and_tag_values_to_new_data tags [
        Tag_Id tag_id_trade_breakdown_string, trade_breakdown_results_to_string value_to_extract_1 groups |> String
    ] |> List.singleton

let transform_description = Predefined_Type_1_Transform_Description (["data"], [], ["layers"; "value_to_extract"],
    fun (tags : Built_In_Tags) _ (string_parameters : Transform_String_Parameters_Type) (inputs : Predefined_Transform_Unsynchronized_Inputs_Type) ->
        trade_breakdown_1 tags string_parameters.["layers"] string_parameters.["value_to_extract"] inputs.["data"])
