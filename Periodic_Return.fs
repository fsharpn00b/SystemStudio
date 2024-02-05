(* fsharplint:disable TypeNames RecordFieldNames UnionCasesNames ModuleNames MemberNames ParameterNames PublicValuesNames NonPublicValuesNames
FL0038, FL0039, FL0041, FL0042, FL0045, FL0046, FL0049, FL0050
See
https://fsprojects.github.io/FSharpLint/how-tos/rule-configuration.html#ruleList
*)

module Periodic_Return

// Environment
open System
// StringBuilder
open System.Text

open Tag_Types
open Data
open Built_In_Tag
open Custom_Transform_Types
open Transform_Types
open Transform_Helpers

(* Types. *)

(* See also Tag_Types for the following types.
Periodic_Return_Data
Periodic_Return_End_Statistics
Periodic_Return_End_Year
Periodic_Return_End_Month
Periodic_Return_Year_Statistics
Periodic_Return_Statistics
(end)
*)

type private Periodic_Return_Start_Year = {
    year : int
    start : Periodic_Return_Data
}

type private Periodic_Return_Start_Month = {
    month : int
    year : int
    start : Periodic_Return_Data
}

type private Periodic_Return_Transform_State_2 = {
    previous_years : Periodic_Return_End_Year list
    previous_months : Periodic_Return_End_Month list
    start_year : Periodic_Return_Start_Year
    start_month : Periodic_Return_Start_Month
}

type private Periodic_Return_Transform_State_1 = 
    | First_Day
    | Not_First_Day of Periodic_Return_Transform_State_2

(* Helper functions. *)

let private calculate_statistics (start_data : Periodic_Return_Data) (closed_equity : float) (open_equity : float) (closed_plus_open_equity : float) : Periodic_Return_End_Statistics =
    {
        closed_equity_change = closed_equity - start_data.closed_equity
        closed_equity_change_percent = (closed_equity - start_data.closed_equity) / start_data.closed_equity * 100.0
        open_equity_change = open_equity - start_data.open_equity
        open_equity_change_percent = (open_equity - start_data.open_equity) / start_data.open_equity * 100.0
        closed_plus_open_equity_change = closed_plus_open_equity - start_data.closed_plus_open_equity
        closed_plus_open_equity_change_percent = (closed_plus_open_equity - start_data.closed_plus_open_equity) / start_data.closed_plus_open_equity * 100.0
    }

let private update_year (state : Periodic_Return_Transform_State_2) (year : int) (closed_equity : float) (open_equity : float) (closed_plus_open_equity : float) : Periodic_Return_Transform_State_2 =
    {
        previous_years = {
            year = state.start_year.year
            start = state.start_year.start
            end_ = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
            statistics = calculate_statistics state.start_year.start closed_equity open_equity closed_plus_open_equity
        } :: state.previous_years
        previous_months = state.previous_months
        start_year = {
            year = year
            start = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
        }
        start_month = state.start_month
    }

let private update_month (state : Periodic_Return_Transform_State_2) (month : int) (year : int) (closed_equity : float) (open_equity : float) (closed_plus_open_equity : float) : Periodic_Return_Transform_State_2 =
    {
        previous_years = state.previous_years
        previous_months = {
            month = state.start_month.month
            year = state.start_month.year
            start = state.start_month.start
            end_ = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
            statistics = calculate_statistics state.start_month.start closed_equity open_equity closed_plus_open_equity
        } :: state.previous_months
        start_year = state.start_year
        start_month = {
            month = month
            year = year
            start = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
        }
    }

let private get_equity_data (data : Data) : {| closed_equity : float; open_equity : float; closed_plus_open_equity : float |} =
    let closed_equity =
        try extract_float_from_data (Tag_Id tag_id_system_state_closed_equity) data
        with | ex -> invalidOp ($"Failed to extract closed equity from Data. Error:%s{Environment.NewLine}%s{ex.Message}")
    let open_equity =
        try extract_float_from_data (Tag_Id tag_id_system_state_open_equity) data
        with | ex -> invalidOp ($"Failed to extract open equity from Data. Error:%s{Environment.NewLine}%s{ex.Message}")
    let closed_plus_open_equity =
        try extract_float_from_data (Tag_Id tag_id_system_state_closed_plus_open_equity) data
        with | ex -> invalidOp ($"Failed to extract closed plus open equity from Data. Error:%s{Environment.NewLine}%s{ex.Message}")
    {| closed_equity = closed_equity; open_equity = open_equity; closed_plus_open_equity = closed_plus_open_equity |}

let private statistics_to_string (statistics : Periodic_Return_Statistics) : string =
(* We cannot find a way to specify the thousands separator for the integer part of a float using F# string formatting.
See:
https://stackoverflow.com/a/4449135
https://stackoverflow.com/a/40830675
(end)
*)
    let number_format = "{0:#,##0.00}"
    let column_format = "{0,-20}{1,20}{2}"

    let helper (start_equity : float) (end_equity : float) (change : float) (change_percent : float) : string =
        let result_1 = StringBuilder (Environment.NewLine)
        let result_2 = result_1.AppendFormat (column_format, "Starting equity:", String.Format (number_format, start_equity), Environment.NewLine)
        let result_3 = result_2.AppendFormat (column_format, "Ending equity:", String.Format (number_format, end_equity), Environment.NewLine)
        let result_4 = result_3.AppendFormat (column_format, "Equity change:", String.Format (number_format, change), Environment.NewLine)
        let result_5 = result_4.AppendFormat (column_format, "Equity change (%):", String.Format (number_format, change_percent), Environment.NewLine)
        result_5.ToString ()

    let result_1 = (StringBuilder (Environment.NewLine), statistics.years) ||> List.fold (fun acc_1 year ->
        let acc_2 = acc_1.AppendFormat ("Year: {0}{1}", year.year.year, Environment.NewLine)
        let acc_3 = acc_2.AppendFormat ("{0}{1}", (helper
            year.year.start.closed_plus_open_equity
            year.year.end_.closed_plus_open_equity
            year.year.statistics.closed_plus_open_equity_change
            year.year.statistics.closed_plus_open_equity_change_percent), Environment.NewLine)
        (acc_3, year.months) ||> List.fold (fun acc_4 month ->
            let acc_5 = acc_4.AppendFormat ("Month: {0}{1}", month.month, Environment.NewLine)
            let acc_6 = acc_5.AppendFormat ("{0}{1}", (helper
                month.start.closed_plus_open_equity
                month.end_.closed_plus_open_equity
                month.statistics.closed_plus_open_equity_change
                month.statistics.closed_plus_open_equity_change_percent), Environment.NewLine)
            acc_6
        )
    )
    result_1.ToString ()

(* Main functions. *)

let private handle_first_day
    (closed_equity : float)
    (open_equity : float)
    (closed_plus_open_equity : float)
    (date_time : DateTime)
    : Periodic_Return_Transform_State_2 =
    {
        previous_years = []
        previous_months = []
        start_year = {
            year = date_time.Year
            start = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
        }
        start_month = {
            month = date_time.Month
            year = date_time.Year
            start = {
                closed_equity = closed_equity
                open_equity = open_equity
                closed_plus_open_equity = closed_plus_open_equity
            }
        }
    }

let private periodic_return_transform_3 (tags : Built_In_Tags) (state : Periodic_Return_Transform_State_1) (end_data : Data) (end_date : DateTime) : Data list =
    match state with
    | Not_First_Day state_2 ->
        let state_3 =
(* If we have not recorded the statistics for the last year, do so. *)
            if state_2.previous_years |> List.exists (fun previous_year -> previous_year.year = end_date.Year) then state_2
            else
                let equity_data = get_equity_data end_data
                update_year state_2 end_date.Year equity_data.closed_equity equity_data.open_equity equity_data.closed_plus_open_equity
        let state_4 =
(* If we have not recorded the statistics for the last month, do so. *)
            if state_3.previous_months |> List.exists (fun previous_month ->
                previous_month.month = end_date.Month && previous_month.year = end_date.Year
            ) then state_3
            else
                let equity_data = get_equity_data end_data
                update_month state_3 end_date.Month end_date.Year equity_data.closed_equity equity_data.open_equity equity_data.closed_plus_open_equity
        let statistics =
            {
                years = state_4.previous_years
(* Sort the statistics by year. *)
                    |> List.sortBy (fun year -> year.year)
                    |> List.map (fun year ->
                        {
                            year = year
(* For each year, sort the statistics by month. *)
                            months = state_4.previous_months
                                |> List.filter (fun month -> month.year = year.year)
                                |> List.sortBy (fun month -> month.month)
                        }
                    )
            }
        add_tags_to_output_data_for_post_system_evaluation_transform tags end_data [
            Tag_Id tag_id_periodic_return, Tag_Value.Periodic_Return statistics
            Tag_Id tag_id_periodic_return_string, Tag_Value.String (statistics_to_string statistics)
        ]
    | _ -> invalidOp ("After evaluating the transform, the transform state is not Not_First_Day.")

let private periodic_return_transform_2 (tags : Built_In_Tags) (data_list : (DateTime * Data) list) : Data list =
    let state_5 = (First_Day, data_list) ||> List.fold (fun state_1 (date_time, data) ->
        let equity_data = get_equity_data data
        match state_1 with
        | First_Day -> handle_first_day equity_data.closed_equity equity_data.open_equity equity_data.closed_plus_open_equity date_time |> Not_First_Day
(* If this is not the first day, it remains not the first day. *)
        | Not_First_Day state_2 ->
            let state_3 =
                if state_2.start_year.year = date_time.Year then state_2
(* If the year has changed, record the statistics for the previous year. *)
                else update_year state_2 date_time.Year equity_data.closed_equity equity_data.open_equity equity_data.closed_plus_open_equity
            let state_4 =
(* If the month has changed, record the statistics for the previous month. *)
                if state_3.start_month.month = date_time.Month then state_3
                else update_month state_3 date_time.Month date_time.Year equity_data.closed_equity equity_data.open_equity equity_data.closed_plus_open_equity
            state_4 |> Not_First_Day
    )
(* We validate the input data list is not empty in Transform.run_transform and Transform_Input.transform_input_2_to_transform_input_3, so it is safe to apply List.last. *)
    let end_date_time, end_data = data_list |> List.last
    periodic_return_transform_3 tags state_5 end_data end_date_time

let private periodic_return_transform_1 (tags : Built_In_Tags) (data_list_1 : Data list) : Data list =
    data_list_1
(* Filter for system-level results. *)
    |> filter_data_list_by_tag_instances [Tag_Id tag_id_system_evaluation_result_type, System_Evaluation_Result_Type System_Evaluation_Result_System]
(* For each Data, extract date/time. *)
    |> List.map (fun data ->
        let date_time =
            try extract_date_time_from_data (Tag_Id tag_id_date_time) data
            with | ex -> invalidOp (sprintf "Failed to extract date/time from Data. Error:%s%s" Environment.NewLine ex.Message)
        date_time, data
    )
(* Sort Data list by date/time. *)
    |> List.sortBy fst
    |> periodic_return_transform_2 tags

(* Transform description. *)

let transform_description = Predefined_Type_1_Transform_Description (["data"], [], [], 
    fun (tags : Built_In_Tags) _ _ (inputs : Predefined_Transform_Unsynchronized_Inputs_Type) ->
        periodic_return_transform_1 tags inputs.["data"])
